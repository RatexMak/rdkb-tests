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

package com.automatics.rdkb.tests.cdl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.providers.imageupgrade.ImageRequestParams;
import com.automatics.providers.imageupgrade.ImageUpgradeMechanism;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;

/**
 * Class to hold all the test scripts for XCONF CDL negative scenarios
 * 
 * @author anandam.s
 * 
 */
public class BroadBandXconfCdlNegativeScenarios extends AutomaticsTestBase {
    /**
     * Verify the Xconf flag support the delay download at installation with invalid images
     * <ol>
     * <li>Configure the mock xconf server for CDL with delayDownload 30 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload 60 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload -3 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload 0 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * </ol>
     * 
     * @param device
     * @author prasanthreddy.a
     * @Refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-XCONF-DELAY-1001")
    public void testToValidateXconfDelay(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-XCONF-DELAY-101";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XCONF-DELAY-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Xconf flag support the delay download at installation with invalid images");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Configure the mock xconf server for CDL with delayDownload 30 mins");
	LOGGER.info("2. Trigger the CDL through webpa");
	LOGGER.info("3. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("4. Configure the mock xconf server for CDL with delayDownload 60 mins");
	LOGGER.info("5. Trigger the CDL through webpa");
	LOGGER.info("6. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("7. Configure the mock xconf server for CDL with delayDownload -3 mins");
	LOGGER.info("8. Trigger the CDL through webpa");
	LOGGER.info("9. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("10. Configure the mock xconf server for CDL with delayDownload 0 mins");
	LOGGER.info("11. Trigger the CDL through webpa");
	LOGGER.info("12. verify the delay download log is logged in xconf.txt.0");

	LOGGER.info("#######################################################################################");

	try {
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_1,
		    BroadBandTestConstants.CONSTANT_30);
	    // Step 4 to Step 6 Validate delay download with 60 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_4,
		    BroadBandTestConstants.CONSTANT_60);
	    // Step 7 to Step 9 Validate delay download with -3 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_7,
		    BroadBandTestConstants.CONSTANT_NEGATIVE_3);
	    // Step 10 to Step 12 Validate delay download with 0 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_10,
		    BroadBandTestConstants.CONSTANT_0);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-DELAY-1001");
    }

    /**
     * Helper Method to validate delay download validations
     * 
     * @param device
     *            Dut instance
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            String testcaseid
     * @param stepNumber
     *            int stepnumber
     * @param delayDownload
     *            int delaydownload
     */
    public void helperMethodtoValidateXconfDelayDownload(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
	    int stepNumber, int delayDownload) {
	// Variable Declaration begins

	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// download protocol
	String downloadProtocol = AutomaticsConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP;
	// Variable Declaration Ends
	String response = null;
	stepNum = "s" + stepNumber;
	errorMessage = "failed to configure the mock xconf details";
	status = false;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Configure the mock xconf server for CDL with delayDownload"
		+ delayDownload + " mins");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Configure mock xconf similar to CDL process) Update /nvram/swupdate.conf file with mock xconf url");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Mock xconf configuration should be successful");
	LOGGER.info("**********************************************************************************");

	try {

	    BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device,
		    BroadBandTestConstants.TEMPORARY_FOLDER, false, downloadProtocol, delayDownload);
	    status = true;
	} catch (Exception e) {
	    errorMessage = "Exception occured while configuring xconf server " + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL: Successfully Configured XCONF server with required configuration.");
	} else {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to execute webpa command";
	status = false;

	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Trigger the CDL through webpa");
	LOGGER.info(
		"STEP " + stepNumber + ": ACTION : Execute command to set value true to Webpa param for trigger CDL");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa should be successful");
	LOGGER.info("**********************************************************************************");

	try {
	    status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
	} catch (Exception e) {
	    errorMessage = "Exception occured while initiating cdl throgh webpa " + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully initiated cdl using webpa command.");
	} else {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL :" + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Failed to verify the delay download log message";
	status = false;
	response = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("STEP " + stepNumber + ": ACTION : grep -I \"Device configured with download delay of "
		+ delayDownload + " minutes\" or Resetting the download delay to 0 minutes /rdklogs/logs/xconf.txt.0");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device configured with download delay of " + delayDownload
		+ " minutes log should be present in xconf.txt.0");
	LOGGER.info("**********************************************************************************");
	delayDownload = delayDownload <= 0 ? BroadBandTestConstants.CONSTANT_0 : delayDownload;
	String validateText = delayDownload == 0 ? BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_RESET
		: BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
			.replace(BroadBandTestConstants.STRING_REPLACE, String.valueOf(delayDownload));
	try {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, validateText,
		    BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, validateText);
	} catch (Exception e) {
	    LOGGER.error("Exception occured while executing command:" + e.getMessage());
	}
	if (status && delayDownload == 0) {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.LOG_CDL_STARTED,
		    BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.LOG_CDL_STARTED);

	}

	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Delay download log validated : " + validateText);
	} else {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

    }

    /**
     * Verify CDL fails for corrupt build generated from same device class
     * <ol>
     * <li>Check whether the required corrupt Images are present in the test CDL server <CDL SERVER URL></li>
     * <li>Configure XCONF firmware download details in Mock Server</li>
     * <li>Trigger CDL to this corrupt build using webpa</li>
     * <li>Verify CDL has started</li>
     * <li>Verify CDL has completed</li>
     * <li>Verify Download is not successful</li>
     * <li>Check firmwareDownload status using webpa</li>
     * <li>Reboot the device and wait till the devices acquires IP</li>
     * <li>Verify the latest image version in the device</li>
     * </ol>
     * 
     * @author Anandam.S
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-NEGATIVE-CDL-1000")
    public void verifyCDLFailsWithCorruptBuildOfSameDeviceClass(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-NEGATIVE-CDL-100";
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NEGATIVE-CDL-1000");
	LOGGER.info("TEST DESCRIPTION: Verify CDL fails for corrupt build generated from same device class");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Check whether the required corrupt Images are present in the test CDL server  <CDL SERVER URL>");
	LOGGER.info("3. Configure XCONF firmware download details in Mock Server");
	LOGGER.info("4. Trigger CDL to this corrupt build using webpa");
	LOGGER.info("5. Verify CDL has started ");
	LOGGER.info("6. Verify CDL has completed");
	LOGGER.info("7. Verify Download is not successful");
	LOGGER.info("8. Check firmware Download status using webpa");
	LOGGER.info("9. Reboot the device  and wait till the devices acquires IP");
	LOGGER.info("10. Verify the latest image version in the device ");

	LOGGER.info("#######################################################################################");
	executeCommonStepsForCDL(device, testCaseId,
		BroadbandPropertyFileHandler.getCurruptImageForSameDeviceClass(device),
		BroadbandPropertyFileHandler.getCurruptImageForSameDeviceClassInfo(device));
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NEGATIVE-CDL-1000");

    }

    /**
     * Verify CDL fails for corrupt build generated from different device class
     * <ol>
     * <li>Check whether the required corrupt Images are present in the test CDL server <CDL SERVER URL></li>
     * <li>Configure XCONF firmware download details in Mock Server</li>
     * <li>Trigger CDL to this corrupt build using webpa</li>
     * <li>Verify CDL has started</li>
     * <li>Verify CDL has completed</li>
     * <li>Verify Download is not successful</li>
     * <li>Check firmwareDownload status using webpa</li>
     * <li>Reboot the device and wait till the devices acquires IP</li>
     * <li>Verify the latest image version in the device</li>
     * </ol>
     * 
     * @refactor Said hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-NEGATIVE-CDL-1001")
    public void verifyCDLFailsWithCorruptBuildOfDifferentDeviceClass(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-NEGATIVE-CDL-101";
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NEGATIVE-CDL-1001");
	LOGGER.info("TEST DESCRIPTION: Verify CDL fails for corrupt build generated from different device class");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Check whether the required corrupt Images are present in the test CDL server  <CDL SERVER URL>");
	LOGGER.info("3. Configure XCONF firmware download details in Mock Server");
	LOGGER.info("4. Trigger CDL to this corrupt build using webpa");
	LOGGER.info("5. Verify CDL has started ");
	LOGGER.info("6. Verify CDL has completed");
	LOGGER.info("7. Verify Download is not successful");
	LOGGER.info("8. Check firmware Download status using webpa");
	LOGGER.info("9. Reboot the device  and wait till the devices acquires IP");
	LOGGER.info("10. Verify the latest image version in the device ");

	LOGGER.info("#######################################################################################");
	executeCommonStepsForCDL(device, testCaseId,
		BroadbandPropertyFileHandler.getCurruptImageForDifferentDeviceClass(device),
		BroadbandPropertyFileHandler.getCurruptImageForDifferentDeviceClassInfo(device));
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NEGATIVE-CDL-1001");

    }

    /**
     * Verify CDL fails for corrupt build which is partially downloaded or having CRC error
     * <ol>
     * <li>Check whether the required corrupt Images are present in the test CDL server <CDL SERVER URL></li>
     * <li>Configure XCONF firmware download details in Mock Server</li>
     * <li>Trigger CDL to this corrupt build using webpa</li>
     * <li>Verify CDL has started</li>
     * <li>Verify CDL has completed</li>
     * <li>Verify Download is not successful</li>
     * <li>Check firmwareDownload status using webpa</li>
     * <li>Reboot the device and wait till the devices acquires IP</li>
     * <li>Verify the latest image version in the device</li>
     * </ol>
     * 
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-NEGATIVE-CDL-1002")
    public void verifyCDLFailsWithCorruptBuildOfPartialDownload(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-NEGATIVE-CDL-102";
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NEGATIVE-CDL-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Verify CDL fails for corrupt build which is partially downloaded or having CRC error");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Check whether the required corrupt Images are present in the test CDL server  <CDL SERVER URL>");
	LOGGER.info("3. Configure XCONF firmware download details in Mock Server");
	LOGGER.info("4. Trigger CDL to this corrupt build using webpa");
	LOGGER.info("5. Verify CDL has started ");
	LOGGER.info("6. Verify CDL has completed");
	LOGGER.info("7. Verify Download is not successful");
	LOGGER.info("8. Check firmware Download status using webpa");
	LOGGER.info("9. Reboot the device  and wait till the devices acquires IP");
	LOGGER.info("10. Verify the latest image version in the device ");

	LOGGER.info("#######################################################################################");
	executeCommonStepsForCDL(device, testCaseId,
		BroadbandPropertyFileHandler.getCurruptImageWithPartialDownload(device),
		BroadbandPropertyFileHandler.getCurruptImageWithPartialDownloadInfo(device));
	LOGGER.info("ENDING TEST CASE: TC-RDKB-NEGATIVE-CDL-1002");

    }

    /**
     * This method contains all the common steps for corrupted image CDL
     * 
     * @param device
     *            {@link Dut}
     * @param testCaseId
     *            test case id
     * @param corruptedImageForCDL
     */
    public void executeCommonStepsForCDL(Dut device, String testCaseId, String corruptedImageForCDL,
	    String imageInfoMap) {
	String buildImageWithoutExtension = null;
	String initialImageName = null;
	boolean isPostConditionRequired = false;
	String stepNum = "s1";
	String errorMessage = "Required images are not  present  in the test CDL server ";
	boolean status = false;
	
	try {
	    status = CommonUtils.clearLogFile(tapEnv, device, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (status) {
		LOGGER.info(BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0 + " contents are cleared successfully");
	    }
	    buildImageWithoutExtension = BroadBandCommonUtils.getCurrentlyRunningImageVersionUsingWebPaCommand(tapEnv,
		    device);
	    initialImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

	    status = false;
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Check whether the required corrupt Images are present in the test CDL server  <CDL SERVER URL>");
	    LOGGER.info(
		    "STEP 1: ACTION : If the test is executed on a given model, the expected corrupt Image name should be presentin test CDL server  ");
	    LOGGER.info("STEP 1: EXPECTED : Required images are present  in the test CDL server  <CDL SERVER URL>");
	    LOGGER.info("**********************************************************************************");
	    try {
		LOGGER.info("Image INFO : \n");
		LOGGER.info(imageInfoMap);
		LOGGER.info("Checking whether the image is available in test CDL server  <CDL SERVER URL>");
	    } catch (Exception e) {
		errorMessage = "Exception occured while retrieving images and info " + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (CommonMethods.isNotNull(corruptedImageForCDL)) {
		status = BroadBandXconfCdlUtils.isImageAvailabeInInvalidImageServer(tapEnv, device,
			corruptedImageForCDL);
	    } else {
		errorMessage = "Could not get image mapping for model " + device.getModel();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL :Required corrupted image is present in Invalid images test CDL server. The image for doing CDL is "
				+ corruptedImageForCDL);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Xconf url  is not configured successfully";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Configure /nvram/swupdate.conf file with Mock Xconf url ");
	    LOGGER.info("STEP 2: ACTION : execute echo \"<XCONF URL>\" > /nvram/swupdate.conf");
	    LOGGER.info("STEP 2: EXPECTED : Xconf url should be configured successfully");
	    LOGGER.info("**********************************************************************************");

	    try {
		// Configure /nvram/swupdate.conf file with Mock Xconf url
		BroadBandXconfCdlUtils.updateSoftwareUpdateConfigurationOnClient(tapEnv, device);
		status = true;
		LOGGER.info("Successfully configured /opt/swupdate.conf with Mock Xconf url");
	    } catch (Exception e) {
		status = false;
		errorMessage = "Exception while configuring mock server." + e.getMessage();
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully configured /nvram/swupdate.conf with Mock Xconf url");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Failed to configure the firmware download details";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Configure XCONF firmware download details in Mock Server");
	    LOGGER.info(
		    "STEP 3: ACTION : set reboot immediately- false  protocol -http ,build name, CDL server  as \"<CDL SERVER URL>\"");
	    LOGGER.info("STEP 3: EXPECTED : Configuration should be successful ");
	    LOGGER.info("**********************************************************************************");

	    try {

		String firmwareLocation = BroadbandPropertyFileHandler.getXconfFirmwareLocationForCorruptImages();

		ImageRequestParams imageRequest = new ImageRequestParams();
		imageRequest.setFirmwareToBeDownloaded(corruptedImageForCDL);
		imageRequest.setRebootImmediately(false);
		imageRequest.setFirmwareLocation(firmwareLocation);

		String response = tapEnv.performImageUpgrade(device, imageRequest, ImageUpgradeMechanism.XCONF);
		LOGGER.info(response);
		status = true;
	    } catch (Exception e) {
		status = false;
		errorMessage = "Exception while configuring firmware download details in mock server." + e.getMessage();
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : SUCCESSFULLY CONFIGURED XCONF FIRMWARE DOWNLOAD DETAILS IN MOCK SERVER");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Failed to trigger CDL using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Trigger CDL to this corrupt build using webpa");
	    LOGGER.info("STEP 4: ACTION : set webpa Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow as true");
	    LOGGER.info("STEP 4: EXPECTED : Webpa should be set as true and CDL should be triggered");
	    LOGGER.info("**********************************************************************************");

	    // Set Xconf Check Now parameter to trigger the CDL
	    WebPaServerResponse response = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    TR69ParamConstants.TR69_XCONF_CHECK_NOW, BroadBandTestConstants.TRUE,
		    AutomaticsConstants.CONSTANT_3);
	    status = (response.getStatusCode() == BroadBandTestConstants.SUCCESS_CODE);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Download triggered  using webpa");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Expected message \"### httpdownload started ###\"  is not  present in   xconf logs at  /rdklogs/logs/xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify CDL has started ");
	    LOGGER.info(
		    "STEP 5: ACTION : Check for message ### httpdownload started ### in xconf logs at /rdklogs/logs/xconf.txt.0");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Expected message \"### httpdownload started ###\"  should be present in   xconf logs at  /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("**********************************************************************************");

	    Map<String, Boolean> cdlLogsStatus = BroadBandXconfCdlUtils.getAllLogsForCDLWithCorruptImages(tapEnv,
		    device, corruptedImageForCDL);

	    status = cdlLogsStatus.get(BroadBandTestConstants.LOG_CDL_STARTED);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verified that CDL has started. The Expected log "
			+ BroadBandTestConstants.LOG_CDL_STARTED + " is found in /rdklogs/logs/xconf.txt");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Expected message \"### httpdownload completed###\"  is not  present in   xconf logs at  /rdklogs/logs/xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify CDL has completed");
	    LOGGER.info(
		    "STEP 6: ACTION : Check for message ### httpdownload completed ### in xconf logs at /rdklogs/logs/xconf.txt.0");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Expected message \"### httpdownload completed ###\"  should be present in   xconf logs at  /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("**********************************************************************************");

	    status = cdlLogsStatus.get(BroadBandTestConstants.LOG_CDL_COMPLETED);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified that CDL has completed. The Expected log "
			+ BroadBandTestConstants.LOG_CDL_COMPLETED + " is found in /rdklogs/logs/xconf.txt");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    String logToCheck = (BroadbandPropertyFileHandler.isDeviceCheckForNegetiveCdl(device)
		    || DeviceModeHandler.isFibreDevice(device))
			    ? BroadBandTestConstants.LOG_CDL_SUCCESSFUL.toLowerCase()
			    : BroadBandTraceConstants.LOG_MESSAGE_HTTP_DOWNLOAD_NOT_SUCCESSFUL;
	    errorMessage = "Expected message " + logToCheck
		    + "  is not  present in   xconf logs at  /rdklogs/logs/xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify " + logToCheck);
	    LOGGER.info(
		    "STEP 7: ACTION : Check for message " + logToCheck + " in xconf logs at /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP 7: EXPECTED : Expected message " + logToCheck
		    + "  should be present in   xconf logs at  /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("**********************************************************************************");

	    status = cdlLogsStatus.get(logToCheck);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified that CDL is not successful. The Expected log "
			+ logToCheck + " is found in /rdklogs/logs/xconf.txt");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Webpa for checking firmware download status  did not  return the expected value ";
	    status = false;
	    String stateToCheck = BroadbandPropertyFileHandler.isDeviceCheckForNegetiveCdl(device)
		    ? BroadBandTestConstants.STRING_FIRMWARE_DOWNLOAD_COMPLETED
		    : BroadBandTestConstants.STRING_FIRMWARE_DOWNLOAD_FAILED;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Check firmwareDownload status using webpa");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute webpa get  Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadStatus");
	    LOGGER.info("STEP 8: EXPECTED : Webpa should return " + stateToCheck);
	    LOGGER.info("**********************************************************************************");

	    String downloadStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_DeviceInfo_X_RDKCENTRAL_COM_FirmwareDownloadStatus);
	    status = downloadStatus.contains(stateToCheck);
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Verified that firmware download status is in state \"FAILED\" using webpa "
				+ BroadBandWebPaConstants.WEBPA_PARAM_Device_DeviceInfo_X_RDKCENTRAL_COM_FirmwareDownloadStatus);
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Failed to get IP after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Reboot the device  and wait till the devices acquires IP");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute /sbin/reboot .Wait till for Maximum 6 minutes for the device to become up after reboot");
	    LOGGER.info("STEP 9: EXPECTED : device should acquire ip after reboot within 6 minutes");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Device has rebooted and has come online");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "firmware has changed in the device after xconf CDL was trigerred with corrupted image ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify the latest image version in the device ");
	    LOGGER.info("STEP 10: ACTION : Execute head /version.txt in RDKB device ");
	    LOGGER.info(
		    "STEP 10: EXPECTED : The command output should contain the previous image before  Xconf CDL was triggered");
	    LOGGER.info("**********************************************************************************");

	    BroadBandResultObject result = BroadBandCommonUtils.verifyImageVersionUsingWebPaCommand(tapEnv, device,
		    buildImageWithoutExtension);

	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (!status) {
		isPostConditionRequired = true;
	    }
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : The image before and after CDL is same. The CDL  with corrupt images has failed as expected");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
	    tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_REMOVE_WAITING_REBOOT_FLAG);
	    if (isPostConditionRequired) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info(
			"POST-CONDITION : DESCRIPTION :If Image has changes in step 10.Revert back to previous image  ");
		LOGGER.info("POST-CONDITION : ACTION : If Image has changes in step 10.Revert back to previous image ");
		LOGGER.info(
			"POST-CONDITION : EXPECTED : If Image has changes in step 10.Revert back to previous image ");

		BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv, initialImageName);

		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
		}
		LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
    }
}
