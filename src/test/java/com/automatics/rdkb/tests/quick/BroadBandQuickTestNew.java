/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.quick;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.providers.imageupgrade.ImageUpgradeMechanism;
import com.automatics.rdkb.BroadBandDeviceSsidInfo;
import com.automatics.rdkb.BroadBandDeviceStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.WiFiSsidConfigStatus;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRegressionQuickTestUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;

public class BroadBandQuickTestNew extends BaseQuickTest {

    /** Holds the test case ID. */
    private String testId = "TC-RDKB-QUICK-009";

    /** Error message on particular STB. */
    private String errorMessage = null;

    /**
     * Test used for quick validation of RDKB build available in build server. Accept build name as parameter and pass
     * it to test case as system property 'BUILD_NAME'. Status of each step will be updated both in test manager and RDK
     * Portal
     * 
     * @author Lakshmi Priya M, Karthick Pandiyan
     * @author Selvaraj Mariyappan
     * @author anandam.s
     * @refactor Govardhan
     * 
     * @param settop
     *            The settop to be used.
     * @throws Exception
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-QUICK-1009")
    public void quickTestValidationOnRDKBdevices(Dut device) {

	// Test step number
	String step = "s1";
	// Variable to hold the server response
	String response = null;
	String deviceMacAddress = device.getHostMacAddress();
	boolean isSnmpTriggeredDownload = false;
	boolean downloadCompletedStatus = false;
	boolean cdlStartedStatus = false;
	// Variable to store the router mode status
	String private2GhzSsidNameBeforeCdl = null;
	String private5GhzSsidNameBeforeCdl = null;
	boolean isWebpaCdlSuccess = false;
	boolean isXconfCdlSuccess = false;
	boolean captivePortalMode = false;
	boolean isBusinessGateway = DeviceModeHandler.isBusinessClassDevice(device);
	boolean rfcEffectOnXfinityWifi = true;
	boolean isDownloadCompleted = false;
	boolean status = false;
	Map<String, WiFiSsidConfigStatus> deviceSsidStatus = null;
	BroadBandDeviceSsidInfo deviceInfo = null;
	boolean isDeviceGoodState = true;
	try {

	    setQuickTestStarted(true);
	    LOGGER.info(
		    "################################################## PRE CONDITION START #####################################################################################");
	    LOGGER.info(
		    "#################################################### PRE CONDITION 1 ############################################################");
	    LOGGER.info(
		    "AS A PRECONDITION MAKE SURE DEVICE IS NOT IN CAPTIVE PORTAL MODE. IF IT IS ENABLED, DISABLE AND PERSONALIZE USING WEBPA PARAMS");
	    LOGGER.info(
		    "#################################################### PRE CONDITION 1 ############################################################");

	    try {

		captivePortalMode = BroadBandWiFiUtils.verifyCaptivePortalModeUsingWebPaCommand(tapEnv, device);

		if (!captivePortalMode) {
		    LOGGER.info("SUCCESSFULLY VERIFIED DEVICE IS NOT ENABLED WITH CAPTIVE PORTAL MODE !!!");
		} else {
		    LOGGER.error(
			    "DEVICE IS IN CAPTIVE PORTAL MODE. SO NEED TO DISABLE CAPTIVE PORTL MODE AND WIFI PERSONALIZATION");
		    // It may take 2-3 minutes for activation
		    BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
		    captivePortalMode = BroadBandWiFiUtils.verifyCaptivePortalModeUsingWebPaCommand(tapEnv, device);
		}
	    } catch (Exception exception) {
		LOGGER.error("Exception occurred while disabling captive portal mode and WiFi personalization : "
			+ exception.getMessage());
	    }

	    if (captivePortalMode) {
		errorMessage = "DEVICE '" + deviceMacAddress
			+ "' IS IN CAPTIVE PORTAL MODE, SO SKIPPING THE QUICK TEST. "
			+ "PLEASE MAKE SURE THAT DEVICE IS NOT IN CAPTIVE PORTAL MODE BEFORE TRIGGERING ANY TEST ON THIS DEVICE";
		LOGGER.error(errorMessage);
		throw new TestException(errorMessage);
	    }

	    // Remove rfc overrides
	    tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVED_RFC_OVERRIDE);

	    // Check the RFC configurations from Xfinity WiFi status
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_RFC_VALUE_CONFIG_FILE);

	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response,
			BroadBandTestConstants.PATTERN_GET_XFINITY_WIFI_STATUS_FROM_RFC_CONFIG);
		if (CommonMethods.isNotNull(response)) {
		    rfcEffectOnXfinityWifi = Boolean.parseBoolean(response.trim());
		    LOGGER.info("Xfinity WiFi status in RFC configuration : " + rfcEffectOnXfinityWifi);
		}
	    }

	    deviceInfo = BroadBandRegressionQuickTestUtils.getBroadBandDeviceSsidInfo(device);
	    deviceSsidStatus = BroadBandRegressionQuickTestUtils.setRequiredPreconditionForQuickTest(device, tapEnv,
		    deviceInfo, false);
	    BroadBandDeviceStatus deviceStatus = BroadBandCommonUtils.getDeviceStatusViaWebPaOrDmcli(device, tapEnv,
		    false);

	    if (!deviceStatus.isRouterModeStatus()) {
		isDeviceGoodState = false;
		errorMessage = "Device is not in router mode and changing to rounter mode using "
			+ BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS
			+ " also failed. Hence device is not in good condition to proceed with test.";
	    }

	    else if (!deviceStatus.isPrivateWiFi2GhzStatus()) {
		isDeviceGoodState = false;
		errorMessage = "Device is not enabled with 2.4 GHz private SSID, enable using TR-181 parameter also failed. Hence device is not in good condition to proceed with quick test.";
	    } else if (!deviceStatus.isPrivateWiFi5GhzStatus()) {
		isDeviceGoodState = false;
		errorMessage = "Device is not enabled with 5 GHz private SSID, enable using TR-181 parameter also failed.Hence device is not in good condition to proceed with quick test.";
	    }
	    if (!isDeviceGoodState) {
		throw new TestException(errorMessage);
	    }

	    LOGGER.info("######################### PRE CONDITION  ############################");
	    LOGGER.info("AS A PRECONDITION Get all the process details before CDL ");
	    LOGGER.info("######################### PRE CONDITION  ############################");

	    BroadBandResultObject processDetailsBeforeUpgrade = BroadBandCommonUtils
		    .verifyAllRequiredBroadBandProcessesForQt(device, tapEnv);
	    BroadBandResultObject processDetailsInAtomBeforeUpgrade = new BroadBandResultObject();
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		processDetailsInAtomBeforeUpgrade = BroadBandCommonUtils
			.validateAllRequiredProcessOnAtomSideForQt(tapEnv, device);
	    }

	    LOGGER.info(
		    "################################################## PRE CONDITION END #####################################################################################");

	    String buildImageWithoutExtension = BroadBandCommonUtils
		    .removeDifferentSignedExtensionsInRequestedBuildName(buildImageName);
	    LOGGER.info("Requested firmware image name without any extension : " + buildImageWithoutExtension);

	    String currentFirmwareVersion = device.getFirmwareVersion();
	    LOGGER.info("Currently Running firmware version  : " + currentFirmwareVersion);

	    if (!buildImageWithoutExtension.equalsIgnoreCase(currentFirmwareVersion)) {

		LOGGER.info("Going to verify the build availability in DAC 15 and PROD CDL SERVER");
		LOGGER.info(
			"********************************************************************************************************");
		/**
		 * Step 1 : Retrieve the requested image name from system properties. Extract exact image name from
		 * different signed names
		 * 
		 */
		status = false;
		step = "s1";
		errorMessage = BroadBandTestConstants.BUILD_NOT_AVAILABLE.replace("<buildImageName>", buildImageName);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 1: DESCRIPTION : Verification of requested firmware version in DAC15 CDL server");
		LOGGER.info(
			"STEP 1: ACTION :  Perform HTTP GET operation using DAC15 Server URL and confirm HTTP Status 200/201 received.\r\t"
				+ "E.g : curl -fLgo <Image server URL>/<firmware_file_name.bin>");
		LOGGER.info("STEP 1: EXPECTED :  Requested firmware version should be available in DAC15 CDL server");
		LOGGER.info("**********************************************************************************");
		status = tapEnv.isImageAvailableInCDL(device, ImageUpgradeMechanism.XCONF, currentFirmwareVersion);

		if (status) {
		    LOGGER.info("STEP 1 : ACTUAL : Build '" + buildImageName
			    + "' is available in dac15/prod cdl server(s), proceeding with code download");
		} else {
		    errorMessage = "Build '" + buildImageName
			    + "' is not available in both dac15 and prod cdl server(s), please check the jenkins status in rdkportal whether is success and build is uploaded to cdl servers";
		    LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
		}

		updateExecutionStatus(device, testId, step, status, errorMessage, true);

		LOGGER.info(
			"********************************************************************************************************");

		LOGGER.info("Going to Upgrade/Downgrade the device " + deviceMacAddress + " with image version '"
			+ buildImageName + " from currently running image version '" + currentFirmwareVersion);

		/**
		 * Step 2 : Trigger TR-181/WebPA HTTP or XCONF HTTP or SNMP TFTP Code download
		 */
		step = "s2";
		errorMessage = BroadBandTestConstants.TEST_STATUS_XCONF_CDL_TRIGGER_FAILED.replace("<buildImageName>",
			buildImageName);
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 2: DESCRIPTION : Trigger XCONF-HTTP firmware download and verification of download started status");
		LOGGER.info("STEP 2: ACTION :  a) Configure Requested firmware Version in XCONF server.\r\t"
			+ "b) Override /nvram/swupdate.conf with  XCONF simulator server URL \r\t"
			+ "Ex: echo \"xconf simulator url\" > /nvram/swupdate.conf'\r\t"
			+ "c) Trigger XCONF-HTTP Code download by setting TR-181(Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow) to true either via WebPA or DMCLI\r\t"
			+ "d) Verify whether XCONF HTTP download triggered using /rdklogs/logs/xconf.txt.0");
		LOGGER.info(
			"STEP 2: EXPECTED :  a) Requested firmware version should be configured properly in XCONF server.\r\t"
				+ "b) /nvram/swupdate.conf should be updated  with XCONF  simulator\r\t"
				+ "c) Device should trigger the firmwareDwnld.sh upon successful execution of TR-181(Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow)\r\t"
				+ "d) \"processing upgrade/downgrade\"  log message should be present in  /rdklogs/logs/xconf.txt.0");
		LOGGER.info("**********************************************************************************");
		/*
		 * Device is loaded with PROD Image which doesn't accept TR-181/WebPA HTTP or XCONF CDL from CI XCONF.
		 * Skip the TR-181/WebPA HTTP or XCONF CDL and perform SNMP based CDL.
		 */
		if (!BroadBandCommonUtils.isProdBuildOnDevice(device)) {

		    tapEnv.executeCommandUsingSsh(device,
			    "rm -rf /tmp/.waitingreboot /tmp/.downloadingfw /tmp/.downloadBreak /tmp/AbortReboot /tmp/.deferringreboot");

		    try {
			cdlStartedStatus = FirmwareDownloadUtils.triggerXconfDownloadAndVerifyDownloadStarted(tapEnv,
				device, buildImageName, true);
			LOGGER.info("Code Download Started Status after XconfDownload : "+cdlStartedStatus);
			// Double check with version of currently running image name vs requested one
			// before
			// proceeding with XCONF trigger. This helps to understand whether its actual
			// failure
			// or because of Issue related with WebPA command.
			if (!cdlStartedStatus) {
			    cdlStartedStatus = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device,
				    buildImageWithoutExtension);
			    LOGGER.info("Verify Image Version From VersionText after XconfDownload : "+cdlStartedStatus);
			    if (cdlStartedStatus) {
				// Already flashed with requested image : No need to check download completed
				// status. Directly go to step 5 and marking S3 and S4 as true.
				isDownloadCompleted = true;
				LOGGER.info("Download completion Status XconfDownload : "+isDownloadCompleted);
			    }
			}
		    } catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }

		    if (!cdlStartedStatus) {
			errorMessage = "HTTP CODE DOWNLOAD USING XCONF WEBPA PARAMETER IS FAILED. HENCE TRYING XCONF HTTP CODE DOWNLOAD";
			LOGGER.error(errorMessage);

			try {
			    cdlStartedStatus = BroadBandCodeDownloadUtils
				    .triggerHttpCodeDownloadUsingWebpaParameters(device, tapEnv, buildImageName);
			    LOGGER.info("Code Download Started Status after Webpa : "+cdlStartedStatus);
			} catch (Exception exception) {
			    cdlStartedStatus = false;
			    errorMessage = exception.getMessage();
			    LOGGER.error(errorMessage);
			}
			if (!cdlStartedStatus) {
			    // Seems like device reboot comes up with requested Image, so marking QT as
			    // "Success".
			    cdlStartedStatus = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device,
				    buildImageWithoutExtension);
			    LOGGER.info("Verify Image Version From VersionText after Webpa : "+cdlStartedStatus);
			}

			if (cdlStartedStatus) {
			    LOGGER.info("SUCCESSFULLY LOADED DEVICE WITH REQUESTED IMAGE");
			    isDownloadCompleted = true;
			    isWebpaCdlSuccess = true;
			} else {
			    errorMessage = "HTTP CODE DOWNLOAD USING HTTP ALSO FAILED. HENCE TRIGGERING SNMP TFTP DOWNLOAD";
			    LOGGER.error(errorMessage);
			}
		    } else {
			LOGGER.info("SUCCESSFULLY TRIGGERED XCONF CODE DOWNLOAD WITH REQUESTED BUILD!!!");
			isXconfCdlSuccess = true;
		    }

		} else {
		    LOGGER.error(
			    "DEVICE WAS LOADED WITH PROD BUILD, SKIP THE TR-181 WEBPA AND XCONF DOWNLOAD AND DIRECTLY TRIGGERRING SNMP TFTP DOWNLOAD");
		}

		/*
		 * Try with SNMP code download if XCONF download failed. This type of download takes more than 15 to 20
		 * minutes to complete. fibre devices not supporting conventional SNMP download.
		 */
		if (!cdlStartedStatus) {
		    LOGGER.info("Going to start SNMP TFTP CDL .");
		    if (!DeviceModeHandler.isFibreDevice(device) && !DeviceModeHandler.isDSLDevice(device)) {

			cdlStartedStatus = FirmwareDownloadUtils
				.triggerAndWaitForTftpCodeDownloadUsingDocsisSnmpCommand(tapEnv, device, buildImageName,
					true);

			if (cdlStartedStatus) {
			    isSnmpTriggeredDownload = true;
			    isDownloadCompleted = true;
			} else {
			    errorMessage = "DOCSIS TFTP SNMP CODE DOWNLOAD ALSO FAILED. NOT ABLE TO DO CODE DOWNLOAD ON TOP OF "
				    + currentFirmwareVersion + " IMAGE USINF WEBPA/XCONF/SNMP CDL METHOD";
			    LOGGER.error(errorMessage);
			}
		    } else {
			errorMessage = "SNMP TFTP CDL IS NOT APPLICABLE FOR " + device.getModel()
				+ " DEVICE. HENCE SKIPPING QUICK TEST";
			LOGGER.error(errorMessage);
		    }
		}
		 LOGGER.info("Code Download Started after Xconf, Webpa and SNMP : "+cdlStartedStatus);
		 LOGGER.info("Code Download Completed status after Xconf, Webpa and SNMP : "+isDownloadCompleted);
		
		if (cdlStartedStatus) {
		    LOGGER.info(
			    "STEP 2 : ACTUAL :SUCCESSFULLY TRIGGERED THE "
				    + (isSnmpTriggeredDownload ? "DOCSIS SNMP TFTP "
					    : (isWebpaCdlSuccess ? "TR-181/WEBPA HTTP" : "XCONF HTTP "))
				    + "CODE DOWNLOAD");
		} else {
		    LOGGER.error("STEP 2 : ACTUAL  :" + errorMessage);
		}
		updateExecutionStatus(device, testId, step, cdlStartedStatus, errorMessage, true);
		LOGGER.info(
			"********************************************************************************************************");

		/*
		 * Verify code download triggered status
		 */
		step = "s3";
		errorMessage = "XCONF HTTP code download is failed during download/upgrade Currently running image <currentFirmwareVersion> is problematic which prevents further download  with requested image <buildImageName>"
			.replace("<buildImageName>", buildImageName);
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 3: DESCRIPTION : Verification of firmware download  completed status for XCONF HTTP code download");
		LOGGER.info(
			"STEP 3: ACTION :  a) Verify whether XCONF HTTP firmware download started using /rdklogs/logs/xconf.txt.0\r\t"
				+ "b) Verify whether XCONF HTTP firmware download completed using /rdklogs/logs/xconf.txt.0\r\t"
				+ "c) Verify whether device is still in maintenance window using  /rdklogs/logs/xconf.txt.0");
		LOGGER.info(
			"STEP 3: EXPECTED :  a) \"XCONF SCRIPT  ### httpdownload started ###\" log message should be present in /rdklogs/logs/xconf.txt.0\r\t"
				+ "b) \"XCONF SCRIPT  ### httpdownload completed ###\" and \"XCONF SCRIPT : HTTP download Successful\" log message should be present in  /rdklogs/logs/xconf.txt.0\r\t"
				+ "c) \"XCONF SCRIPT : Still within current maintenance window for reboot\" log message should be present in /rdklogs/logs/xconf.txt.0");
		LOGGER.info("**********************************************************************************");

		if (!isDownloadCompleted) {

		    if (isXconfCdlSuccess) {
			LOGGER.info("Wait for Xconf DownloadComplete");
			/*
			 * Verify cdl success logs from /rdklogs/logs/xconf.txt.0 for XCONF code download method
			 */
			downloadCompletedStatus = FirmwareDownloadUtils
				.waitForXconfDownloadCompleteAndVerifyDownloadProgressUsingLogs(tapEnv, device,
					buildImageWithoutExtension);
			
			LOGGER.info("DownloadComplete status is : "+downloadCompletedStatus);
			/*
			 * If XCONF HTTP download/upgrade is failed, then try with DOCSIS SNMP Code download.
			 */
			if (!downloadCompletedStatus && !DeviceModeHandler.isDSLDevice(device)) {

			    if (!DeviceModeHandler.isFibreDevice(device)) {
				LOGGER.error(
					"XCONF HTTP CODE DOWNLOAD IS FAILED DURING DOWNLOAD/UPGRADE. TRYING WITH SNMP TFTP CODE DOWNLOAD,"
						+ " IT MAY TAKE MORE THAN 15 TO 20 MINUTES");
				/*
				 * Waiting for 3 minutes after getting the ECM IP address, to make sure that devices are
				 * up after reboot from Above XCONF Trigger, This will helps to rule out other timing
				 * related issue.
				 */
				LOGGER.info(
					"Waiting for 3 more minutes to make sure that device is initialized properly before Triggering SNMP TFTP Code download");
				tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);

				downloadCompletedStatus = FirmwareDownloadUtils
					.triggerAndWaitForTftpCodeDownloadUsingDocsisSnmpCommand(tapEnv, device,
						buildImageName, true);

				if (downloadCompletedStatus) {
				    isSnmpTriggeredDownload = true;
				    isDownloadCompleted = true;
				} else {
				    errorMessage = "XCONF HTTP CODE DOWNLOAD IS FAILED DURING DOWNLOAD/UPGRADE. SO AS A RETRY, DOCSIS TFTP SNMP CODE DOWNLOAD ALSO FAILED. HENCE NOT ABLE TO DO CODE DOWNLOAD ON TOP OF "
					    + currentFirmwareVersion + " IMAGE USINF WEBPA/XCONF/SNMP CDL METHOD";
				    LOGGER.error(errorMessage);
				}
			    }

			} else {
			    LOGGER.info(
				    "SUCCESSFULLY COMPLETED THE XCONF HTTP CODE DOWNLOAD AND VERIFIED THE COMPLETED/UPGRADED STATUS USING DEVICE XCONF LOGS XCONF CDL");
			}
		    } else if (isWebpaCdlSuccess) {
			LOGGER.info(
				"SUCCESSFULLY LOADED DEVICE WITH REQUESTED IMAGE USING TR-181 PARAMATERS VIA WEBPA ");
			downloadCompletedStatus = true;
		    } else if (isSnmpTriggeredDownload) {
			LOGGER.info(
				"SUCCESSFULLY COMPLETED THE SNMP TFTP CODE DOWNLOAD AND VERIFIED THE COMPLETED/UPGRADED STATUS USING SNMP COMMAND(TFTP)");
			downloadCompletedStatus = true;
		    }
		} else {
		    downloadCompletedStatus = true;
		}

		if (downloadCompletedStatus) {
		    LOGGER.info("STEP 3 : ACTUAL : Successfully loaded device with requested image ");
		} else {
		    LOGGER.error("STEP 3 : ACTUAL  :" + errorMessage);
		}
		updateExecutionStatus(device, testId, step, downloadCompletedStatus, errorMessage, true);

		LOGGER.info(
			"********************************************************************************************************");

		/*
		 * Verification of SSH connectivity after successful code download
		 */
		step = "s4";
		status = false;
		errorMessage = "Device is not accessible after upgrading device with " + buildImageName + " image ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 4: DESCRIPTION : Verififcation of Device SSH connectivity ");
		LOGGER.info(
			"STEP 4: ACTION :  a) SSH to device via jump server  and execute command 'echo test_connection'\r\t"
				+ "E.g : sudo stbsshv6 <ipv6_address> \"echo test_connection\"");
		LOGGER.info(
			"STEP 4: EXPECTED :  Device should be accessible via SSH command after successful download");
		LOGGER.info("**********************************************************************************");
		if (!isDownloadCompleted) {
		    try {

			if (isXconfCdlSuccess) {
			    status = BroadBandCommonUtils.isRdkbDeviceAccessible(tapEnv, device,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
				    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, false);
			    if (status) {
				status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
			    } else {
				status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device,
					buildImageWithoutExtension);
				if (!status) {
				    errorMessage = "Device is not rebooting after Xconf http code download with maintanance window";
				}
			    }
			}

		    } catch (Exception exception) {
			status = false;
			errorMessage = exception.getMessage();
		    }
		    if (status) {
			LOGGER.info("SUCCESSFULLY VERIFIED THAT DEVICE REBOOTED  AFTER SUCCESSFUL CODE DOWNLOAD !!!");
		    } else {
			LOGGER.error(errorMessage);
		    }
		} else {
		    status = true;
		}

		if (status) {
		    LOGGER.info(
			    "STEP 4 : ACTUAL : Successfully verified that device rebooted  after successful code download !!!");
		} else {
		    LOGGER.error("STEP 4 : ACTUAL  :" + errorMessage);
		}
		updateExecutionStatus(device, testId, step, status, errorMessage, true);

		LOGGER.info(
			"********************************************************************************************************");

		/**
		 * Step 5: Check version and verify whether requested image is flashed successfully and comes up. using
		 * version.txt
		 */
		status = false;
		step = "s5";
		errorMessage = BroadBandTestConstants.TEST_STATUS_BOX_NOT_FLASHED_WITH_REQUESTED_IMAGE
			.replaceAll("<buildImageName>", buildImageWithoutExtension);
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 5: DESCRIPTION : Check version and verify whether requested image is flashed successfully ");
		LOGGER.info("STEP 5: ACTION :   get the current version from version.txt file");
		LOGGER.info("STEP 5: EXPECTED :  Box should be flashed with requested image");
		LOGGER.info("**********************************************************************************");
		status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device,
			buildImageWithoutExtension);
		if (status) {
		    LOGGER.info(
			    "STEP 5 : ACTUAL : Successfully verified the currently running image version using version.Txt after code download");
		} else {
		    LOGGER.error(" STEP 5 : ACTUAL : " + errorMessage);
		}
		updateExecutionStatus(device, testId, step, status, errorMessage, true);

		LOGGER.info(
			"DUE TO CODE DOWNLOAD, WAITING FOR ADDITIONAL MINITES TO GET INTERFACE DETAILS AND OTHER PROCESS RUNNING STATUS");
		tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
		tapEnv.executeCommandUsingSsh(device, "uptime");
	    } else {
		LOGGER.info(
			"NO NEED TO PERFORM CODE DOWNLOAD, SINCE BOTH CURRENTLY RUNNING IMAGE VERSION AND REQUESTED IMAGE VERSION ARE SAME");
		int testStep = 1;
		LOGGER.info("GOING TO UPDATE S1 To S5 AS PASSED TO PROCEED WITH QUICK TEST");
		while (testStep <= 5) {
		    updateExecutionStatus(device, testId, "s" + testStep, true, errorMessage, true);
		    testStep++;
		}
	    }
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 6: Check whether erouter0 is initialized properly and retrieved IPv4 address.
	     */
	    status = false;
	    step = "s6";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_EROUTER0_IPV4_ERROR;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION :  Check whether erouter0 is initialized properly and retrieved IPv4 address ");
	    LOGGER.info("STEP 6: ACTION :   Execute command \"/sbin/ifconfig erouter0\" and check for IPv4 pattern");
	    LOGGER.info("STEP 6: EXPECTED :  erouter0 should be initialized with respective IPv4 address");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.IFCONFIG_EROUTER);
	    LOGGER.debug("PATTERN: " + BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
	    if (status) {
		LOGGER.info(
			"STEP 6 : ACTUAL :  Successfully verified that erouter0 interface is properly initialized with ipv4 address after code download");
	    } else {
		errorMessage = errorMessage + ", 'ifconfig erouter0' COMMAND RESPONSE : \n " + response;
		LOGGER.error("STEP 6 : ACTUAL : " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 7: Check whether erouter0 is initialized properly and retrieved IPv6 address.
	     */
	    step = "s7";
	    status = false;
	    errorMessage = BroadBandTestConstants.TEST_STATUS_EROUTER0_IPV6_ERROR;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION :  Check whether erouter0 is initialized properly and retrieved IPv6 address ");
	    LOGGER.info("STEP 7: ACTION :   Execute command \"/sbin/ifconfig erouter0\" and check for IPv6 pattern");
	    LOGGER.info("STEP 7: EXPECTED :  erouter0 should be initialized with respective IPv6 address");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.debug("PATTERN: " + BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);

	    if (status) {
		LOGGER.info(
			"STEP 7 : ACTUAL : Successfully verified that erouter0 interface is properly initialized with ipv6 address after code download");
	    } else {
		errorMessage = errorMessage + ", 'ifconfig erouter0' COMMAND RESPONSE : \n " + response;
		LOGGER.error("STEP 7 : ACTUAL : " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 8: Check whether erouter0 MAC is same as that of device WAN MAC Address.
	     */
	    step = "s8";
	    status = false;
	    errorMessage = BroadBandTestConstants.TEST_STATUS_EROUTER0_MAC_ADDRESS_ERROR;
	    String erouter0MacAddress = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION :  Check whether erouter0 MAC Address matches with WAN MAC Address ");
	    LOGGER.info(
		    "STEP 8: ACTION :   Execute command \"/sbin/ifconfig erouter0\" and check for MAC address pattern");
	    LOGGER.info("STEP 8: EXPECTED :  erouter0 MAC Address should be same as that of WAN MAC Address");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isNotNull(response)) {
		erouter0MacAddress = CommonMethods.patternFinder(response,
			BroadBandTestConstants.PATTERN_DEVICE_MAC_ADDRESS_FROM_IFCONFIG_CMD);
	    }
	    LOGGER.debug("erouter0 mac address retrieved from ifconfig command = " + erouter0MacAddress);

	    status = deviceMacAddress.equalsIgnoreCase(erouter0MacAddress);

	    if (status) {
		LOGGER.info(
			"STEP 8 : ACTUAL : Successfully verified that erouter0 mac address is same as that of device wan mac address after code download");
	    } else {
		errorMessage = errorMessage.replaceAll("<wanMacAddress>", deviceMacAddress)
			.replaceAll("<erouter0MacAddress>", erouter0MacAddress)
			+ ", 'ifconfig erouter0' COMMAND RESPONSE : \n " + response;
		LOGGER.error(" STEP 8 : ACTUAL : " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 9 : Check whether brlan0 is initialized properly and retrieved DHCPv4 address.
	     */
	    step = "s9";
	    status = false;
	    errorMessage = BroadBandTestConstants.TEST_STATUS_BRLAN0_DHCPV4_ERROR;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION :  Check whether brlan0 is initialized properly and retrieved DHCPv4 address ");
	    LOGGER.info(
		    "STEP 9: ACTION :   Execute command \"/sbin/ifconfig brlan0\" and check for ipv4 address pattern");
	    LOGGER.info("STEP 9: EXPECTED :  brlan0 should be initialized with respective DHCPv4 address");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.IFCONFIG_BRLAN);
	    LOGGER.debug("PATTERN: " + BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);

	    if (status) {
		LOGGER.info(
			"STEP 9 : ACTUAL : Successfully verified that brlan0 interface is properly initialized with dhcp ipv4 address after code download");
	    } else {
		errorMessage = errorMessage + ", 'ifconfig brlan0' COMMAND RESPONSE : \n " + response;
		LOGGER.error(" STEP 9 : ACTUAL :  " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 10 : Check whether brlan0 is initialized properly and retrieved DHCPv6 address.
	     */
	    step = "s10";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_BRLAN0_DHCPV6_ERROR;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION :  Check whether brlan0 is initialized properly and retrieved DHCPv6 address ");
	    LOGGER.info(
		    "STEP 10: ACTION :   Execute command \"/sbin/ifconfig brlan0\" and check for ipv6 address pattern");
	    LOGGER.info("STEP 10: EXPECTED :  brlan0 should be initialized with respective DHCPv6 address");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PATTERN: " + BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);

	    if (status) {
		LOGGER.info(
			"STEP 10 : ACTUAL : Successfully verified that brlan0 interface is properly initialized with dhcp ipv6 address after code download");
	    } else {
		errorMessage = errorMessage + ", 'ifconfig brlan0' COMMAND RESPONSE : \n " + response;
		LOGGER.error(" STEP 10 : ACTUAL : " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 11 : Check SNMP functionality - SysDescr
	     */
	    step = "s11";
	    status = false;
	    errorMessage = BroadBandTestConstants.TEST_STATUS_SNMP_FAILURE.replaceAll("<buildImageName>",
		    buildImageWithoutExtension);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION :  Check SNMP functionality ");
	    LOGGER.info("STEP 11: ACTION :  get the current running version using SNMP MIB");
	    LOGGER.info("STEP 11: EXPECTED :  SNMP MIB should provide proper software version");
	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {
		status = BroadBandSnmpUtils.verifyCurrentlyRunningFirmwareVersionUsingSysDescrSnmpCommand(tapEnv,
			device, buildImageWithoutExtension);
		if (status) {
		    LOGGER.info(
			    "STEP 11 : ACTUAL :Successfully verified the snmp connectivity and currently running image version using snmp mib 'sysdescr' after code download");
		} else {
		    LOGGER.error("STEP 11 : ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE,
			"This test step not applicable for DSL devices", false);
	    }
	    LOGGER.info(
		    "********************************************************************************************************");

	    step = "s12";
	    String armOrAtomConsole = (CommonMethods.isAtomSyncAvailable(device, tapEnv) ? "ON ARM SIDE" : "");
	    errorMessage = BroadBandTestConstants.TEST_STATUS_CCSP_COMPONENTS_NOT_INITIALIZED_ERROR
		    .replaceAll("<console>", armOrAtomConsole);
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verification of all required (must have) processes ( ARM in case of AtomSyncAvailable devices )");
	    LOGGER.info("STEP 12: ACTION : a) Execute command \"ps -ww\" (in case of AtomSyncAvailable devices, ARM side)\r\t"
		    + "b) Verify whether all required processes are initialized properly.\r\t"
		    + "c) Verify whether any process initialized more than once ( duplicate )");
	    LOGGER.info("STEP 12: EXPECTED : Device should be initialized properly with all "
		    + "required processes( ARM side in case of AtomSyncAvailable devices ) and there should not be any duplicate process.");
	    LOGGER.info("**********************************************************************************");
	    BroadBandResultObject broadBandResultObject = BroadBandCommonUtils
		    .verifyAllRequiredBroadBandProcessesForQt(device, tapEnv);
	    status = broadBandResultObject.isStatus();
	    List<String> notRunningProcess = new ArrayList<>();
	    String process = "";

	    if (!status) {
		LOGGER.error("ALL PROCESS(S) ARE NOT INITIALIZED PROPERLY, WAIT FOR ANOTHER ONE MIN AND CHECK AGAIN");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		broadBandResultObject = BroadBandCommonUtils.verifyAllRequiredBroadBandProcessesForQt(device, tapEnv);
		status = broadBandResultObject.isStatus();

		if (!status) {

		    notRunningProcess = broadBandResultObject.getNotRunningProcessList();

		    if (notRunningProcess.size() > 0) {
			for (int iteration = 0; iteration < notRunningProcess.size(); iteration++) {

			    status = processDetailsBeforeUpgrade.getNotRunningProcessList()
				    .contains(notRunningProcess.get(iteration));

			    if (status) {
				LOGGER.info(notRunningProcess.get(iteration)
					+ " was not running even before firmware upgrade !!!");
			    } else {
				process += notRunningProcess.get(iteration) + ",";
			    }
			}
			errorMessage = process
				+ " process was running before firmware upgrade. But not initialized after loaded device with  "
				+ buildImageWithoutExtension + " build";
		    } else {
			errorMessage = broadBandResultObject.getErrorMessage();
		    }
		}
	    }
	    errorMessage = errorMessage + " " + broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 12 : ACTUAL : Successfully verified all critical ccsp processes  " + armOrAtomConsole);
	    } else {
		LOGGER.error("STEP 12 : ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    step = "s13";
	    BroadBandTestConstants.TEST_STATUS_CCSP_COMPONENTS_NOT_INITIALIZED_ERROR.replaceAll("<console>",
		    "ATOM SIDE");
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Verification of all required (must have)  processes ( ATOM in case of AtomSyncAvailable devices )\r\t"
			    + "NA for AtomSyncAvailable not available devices ( Applicable only for AtomSyncAvailable devices ).");
	    LOGGER.info("STEP 13: ACTION : a) Execute command \"ps -ww\" (in case of AtomSyncAvailable devices, ATOM side)\r\t"
		    + "b) Verify whether all required processes are initialized properly.\r\t"
		    + "c) Verify whether any process initialized more than once ( duplicate )");
	    LOGGER.info("STEP 13: EXPECTED : Device should be initialized properly "
		    + "with all required processes( ATOM side in case of AtomSyncAvailable devices ) and there should not be any duplicate process.");
	    LOGGER.info("**********************************************************************************");

	    List<String> notRunningProcessInAtom = new ArrayList<>();
	    process = "";

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		broadBandResultObject = BroadBandCommonUtils.validateAllRequiredProcessOnAtomSideForQt(tapEnv, device);
		status = broadBandResultObject.isStatus();

		LOGGER.error(broadBandResultObject.getErrorMessage());
		if (!status) {
		    LOGGER.error(
			    "ALL PROCESS(S) ARE NOT INITIALIZED PROPERLY ON ATOM SIDE, WAIT FOR ANOTHER ONE MINS AND CHECK AGAIN");
		    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    broadBandResultObject = BroadBandCommonUtils.validateAllRequiredProcessOnAtomSideForQt(tapEnv,
			    device);
		    status = broadBandResultObject.isStatus();
		    if (!status) {

			notRunningProcessInAtom = broadBandResultObject.getNotRunningProcessList();

			if (notRunningProcessInAtom.size() > 0) {
			    for (int iteration = 0; iteration < notRunningProcessInAtom.size(); iteration++) {

				status = processDetailsInAtomBeforeUpgrade.getNotRunningProcessList()
					.contains(notRunningProcessInAtom.get(iteration));

				if (status) {
				    LOGGER.info(notRunningProcessInAtom.get(iteration)
					    + " was not running even before firmware upgrade !!!");
				} else {
				    process += notRunningProcessInAtom.get(iteration) + ",";
				}
			    }
			    errorMessage = process
				    + " process was running before firmware upgrade in atom console. But not initialized after loaded device with  "
				    + buildImageWithoutExtension + " build";
			} else {
			    errorMessage = broadBandResultObject.getErrorMessage();
			}
		    }
		}
		if (status) {
		    LOGGER.info(
			    "STEP 13 : ACTUAL : Successfully verified all the must have process are running properly on atom side after firmware upgrade  !!!");
		} else {
		    errorMessage = errorMessage + " " + broadBandResultObject.getErrorMessage();
		    LOGGER.error(" STEP 13 : ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    } else {
		tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE,
			"Process validation on ATOM side is applicable only for Atom Sync devices", false);
	    }
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 14 : Check WebPA connectivity - basic command to get current
	     * version(Device.DeviceInfo.X_CISCO_COM_FirmwareName).
	     */
	    step = "s14";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_WEBPA_COMMUNICATION_ERROR.replaceAll("<buildImageName>",
		    buildImageWithoutExtension);
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION :  Check WebPA connectivity");
	    LOGGER.info("STEP 14: ACTION :   get the current running version using webpa");
	    LOGGER.info("STEP 14: EXPECTED :   WebPA connectivity should be established");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.verifyCurrentlyRunningImageVersionUsingWebPaCommand(tapEnv, device,
		    buildImageWithoutExtension);

	    if (status) {
		LOGGER.info(
			"STEP 14 : ACTUAL : Successfully verified the device webpa's connectivity and currently running image version "
				+ "using tr181 parameter 'device.deviceinfo.x_cisco_com_firmwarename' after code download");
	    } else {
		LOGGER.error(" STEP 14 : ACTUAL : " + errorMessage);
		BroadBandCommonUtils.printDiagnosticInformationOnFailureSteps(device, tapEnv, testId, step);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    Map<String, String> webpaParamsAfterQT = tapEnv.executeMultipleWebPaGetCommands(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIST_QT_POSTCONDITION);

	    /**
	     * Step 15 : Check whether device is in captive portal mode or not after
	     * CDL(Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable).
	     */
	    step = "s15";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_CAPTIVE_PORTAL_AFTER_CDL;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION :  Check whether device is in captive portal mode or not after successful CDL");
	    LOGGER.info(
		    "STEP 15: ACTION :   Execute cwebpa on " + BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE
			    + ", " + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_CONFIGURE_WIFI + ", "
			    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_WIFI_NEEDS_PERSONALIZATION);
	    LOGGER.info("STEP 15: EXPECTED :  Device should not be in captive portal mode after successful CDL");
	    LOGGER.info("**********************************************************************************");

	    boolean captivePortalStatus = true;
	    try {
		boolean captivePortalEnabledStatus = Boolean.parseBoolean(
			webpaParamsAfterQT.get(BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE));
		boolean configureWiFiStatus = Boolean.parseBoolean(webpaParamsAfterQT
			.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_CONFIGURE_WIFI));
		boolean wiFiNeedsPersonalizationStatus = Boolean.parseBoolean(webpaParamsAfterQT
			.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_WIFI_NEEDS_PERSONALIZATION));
		captivePortalStatus = captivePortalEnabledStatus && configureWiFiStatus
			&& wiFiNeedsPersonalizationStatus;
	    } catch (Exception exe) {
		errorMessage = "Exception occurred while querying the Captive portal status using WebPA/Dmcli command  -> "
			+ exe.getMessage();
	    }
	    if (!captivePortalStatus) {
		LOGGER.info(
			"STEP 15 : ACTUAL : Verified that device is not in captive portal enabled mode after successfully cdl");
	    } else {
		LOGGER.error("STEP 15 : ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, !captivePortalStatus, errorMessage, false);
	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 16 : Check whether device is in bridge mode after
	     * CDL(Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode ).
	     */
	    step = "s16";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_BRIDGE_MODE_AFTER_CDL;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION :  Check whether device is in bridge mode or not after successful CDL. ");
	    LOGGER.info("STEP 16: ACTION :   Execute webpa on BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS."
		    + " Expected result is \"router\"");
	    LOGGER.info("STEP 16: EXPECTED :   Device should not be in bridge mode mode after successful CDL");
	    LOGGER.info("**********************************************************************************");
	    status = webpaParamsAfterQT.get(BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS)
		    .contains(BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);

	    if (status) {
		LOGGER.info("STEP 16 : ACTUAL : Verified that device is not in bridge mode after successfully cdl");
	    } else {
		LOGGER.error("STEP 16 : ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 17 : Check whether device is enabled with 2.4 GHz network and is advertised after
	     * CDL(Device.WiFi.SSID.10001.Enable and Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled ).
	     */
	    step = "s17";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_PRIVATE_SSID_2_4_GHZ_NOT_ENABLED_AFTER_CDL;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION : Check whether device is enabled with 2.4 GHz network and is advertised after successful CDL ");
	    LOGGER.info("STEP 17: ACTION :  Check SSID name .enabled status and SSID Advt status");
	    LOGGER.info(
		    "STEP 17: EXPECTED :  Device should be enabled with private SSID 2.4 GHz network and advertised after successful CDL");
	    LOGGER.info("**********************************************************************************");
	    private2GhzSsidNameBeforeCdl = deviceSsidStatus.get("private_2ghz").getName();

	    String private2GhzSsidNameAfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
	    String private2GhzSsidAdvtStatusAfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED);
	    String private2GhzSsidStatusfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE);

	    status = CommonMethods.isNotNull(private2GhzSsidStatusfterCdl)
		    && CommonMethods.isNotNull(private2GhzSsidAdvtStatusAfterCdl)
		    && CommonMethods.isNotNull(private2GhzSsidNameAfterCdl)
		    && CommonMethods.isNotNull(private2GhzSsidNameBeforeCdl)
		    && private2GhzSsidNameAfterCdl.equalsIgnoreCase(private2GhzSsidNameBeforeCdl)
		    && Boolean.parseBoolean(private2GhzSsidAdvtStatusAfterCdl)
		    && Boolean.parseBoolean(private2GhzSsidStatusfterCdl);
	    if (!status) {
		errorMessage = "Some of the 2.4ghz Private WIFI parameters are not as expected after firmware upgrade.2.4ghz Private SSID name after CDL :"
			+ private2GhzSsidNameAfterCdl + " 2.4ghz Private SSID name before CDL :"
			+ private2GhzSsidNameBeforeCdl + " 2.4ghz SSID status after CDL :"
			+ private2GhzSsidStatusfterCdl + " 2.4ghz Private SSID Advt status after CDL : "
			+ private2GhzSsidAdvtStatusAfterCdl;
	    }

	    if (status) {
		LOGGER.info(
			"STEP 17 : ACTUAL : Successfully verified the 2.4ghz private SSID name, enabled status and SSID Advt status!!!");
	    } else {
		LOGGER.error(" STEP 17 : ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    /**
	     * Step 18 : Check whether device is enabled with 5 GHz network is advertised after
	     * CDL(Device.WiFi.SSID.10101.Enable and Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled ).
	     */
	    step = "s18";
	    errorMessage = BroadBandTestConstants.TEST_STATUS_PRIVATE_SSID_5_GHZ_NOT_ENABLED_AFTER_CDL;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION :  Check whether device is enabled with 5 GHz network and is advertised after successful CDL. ");
	    LOGGER.info("STEP 18: ACTION :    Check SSID name .enabled status and SSID Advt status");
	    LOGGER.info(
		    "STEP 18: EXPECTED :  Device should be enabled with private SSID 5 GHz network and advertised after successful CDL");
	    LOGGER.info("**********************************************************************************");
	    private5GhzSsidNameBeforeCdl = deviceSsidStatus.get("private_5ghz").getName();
	    String private5GhzSsidNameAfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
	    String private5GhzSsidAdvtStatusAfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED);
	    String private5GhzSsidStatusfterCdl = webpaParamsAfterQT
		    .get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);

	    status = CommonMethods.isNotNull(private5GhzSsidStatusfterCdl)
		    && CommonMethods.isNotNull(private5GhzSsidAdvtStatusAfterCdl)
		    && CommonMethods.isNotNull(private5GhzSsidNameAfterCdl)
		    && CommonMethods.isNotNull(private5GhzSsidNameBeforeCdl)
		    && private5GhzSsidNameAfterCdl.equalsIgnoreCase(private5GhzSsidNameBeforeCdl)
		    && Boolean.parseBoolean(private5GhzSsidAdvtStatusAfterCdl)
		    && Boolean.parseBoolean(private5GhzSsidStatusfterCdl);
	    if (!status) {
		errorMessage = "Some of the 5ghz Private WIFI parameters are not as expected after firmware upgrade.2.4ghz Private SSID name after CDL :"
			+ private5GhzSsidNameAfterCdl + " 5ghz Private SSID name before CDL :"
			+ private5GhzSsidNameBeforeCdl + " 5ghz SSID status after CDL :" + private5GhzSsidStatusfterCdl
			+ " 5ghz Private SSID Advt status after CDL : " + private5GhzSsidAdvtStatusAfterCdl;
	    }

	    if (status) {
		LOGGER.info(
			"STEP 18 : ACTUAL : Successfully verified the 5ghz private SSID name, enabled status and SSID Advt status!!!");
	    } else {
		LOGGER.error(" STEP 18 : ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    LOGGER.info(
		    "********************************************************************************************************");

	    step = "s20";
	    errorMessage = "Xfinity wifi is not enabled after device upgrade";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION :  Verify Xfinity WiFi enabled status using TR-181 parameters ");
	    LOGGER.info("STEP 20: ACTION :   Execute webpa "
		    + BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI);
	    LOGGER.info("STEP 20: EXPECTED :  Xfinity WiFi enabled status must be true");
	    LOGGER.info("**********************************************************************************");

	    if (!isBusinessGateway) {
		try {
		    String xfinityWifi = webpaParamsAfterQT
			    .get(BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI);

		    if (CommonMethods.isNotNull(xfinityWifi)) {
			status = rfcEffectOnXfinityWifi ? xfinityWifi.equalsIgnoreCase(RDKBTestConstants.TRUE)
				: xfinityWifi.equalsIgnoreCase(RDKBTestConstants.FALSE);
		    }

		    if (status) {
			if (rfcEffectOnXfinityWifi) {
			    LOGGER.info(
				    " STEP 20 : ACTUAL : Successfully verified device is enabled xfinity wifi using tr-181 parameter after firmware upgrade !!!");
			} else {
			    LOGGER.info(
				    " STEP 20 : ACTUAL : Xfinity wifi is set to disabled in RFC. Hence verified that Xfinity wifi is disabled after firmware upgrade !!!");
			}

		    } else {
			errorMessage = "Xfinity wifi  is not in expected state  after firmware upgrade. Xfinity wifi set in RFC :   "
				+ rfcEffectOnXfinityWifi + "  Xfinity wifi afer firmware upgrade " + xfinityWifi;
			LOGGER.error(" STEP 20: ACTUAL : " + errorMessage);
		    }
		} catch (Exception exception) {
		    LOGGER.error(
			    "exception occurred while checking Xfinity Wifi SSID status : " + exception.getMessage());
		}
		tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
	    } else {
		tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE,
			"Xfinity wifi is not applicable for BWG devices", false);
	    }

	    LOGGER.info(
		    "********************************************************************************************************");

	} catch (Exception e) {
	    LOGGER.error("Exception occurred while running step " + step + "::::>>>>" + e.getMessage(), e);
	    updateExecutionStatus(device, testId, step, false, errorMessage + e.getMessage(), false);
	}
    }
}
