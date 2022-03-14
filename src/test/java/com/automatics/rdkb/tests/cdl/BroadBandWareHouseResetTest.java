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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.core.SupportedModelHandler;
import com.automatics.device.Dut;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.xconf.XConfUtils;

public class BroadBandWareHouseResetTest extends AutomaticsTestBase {

    /**
     * Verify warehouse staging using xconf method
     * <ol>
     * <li>1. Get current Image name using webpa</li>
     * <li>2. Trigger CDL upgrade device to latest stable build using mock XCONF with factory reset immediately
     * true</li>
     * <li>3. Verify factoryResetImmediately log message in Xconf log file</li>
     * <li>4. Wait for device to reboot and verify it comes up with requested image</li>
     * <li>5. Verify last reboot reason as factory-rest using webpa</li>
     * </ol>
     * 
     * @author ArunKumar Jayachandran
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WAREHOUSE-1001")
    public void testVerifyWarehouseResetUsingXconf(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WAREHOUSE-101";
	String stepNum = "s1";
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	int postConStepNumber = BroadBandTestConstants.CONSTANT_0;
	String response = null;
	String errorMessage = null;
	boolean status = false;
	boolean isCdlDataPosted = false;
	String currentImage = null;
	String latestImage = null;
	String logMessage = null;
	String buildExtension = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WAREHOUSE-1001");
	LOGGER.info("TEST DESCRIPTION: Verify warehouse staging using xconf method");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get current Image name using webpa");
	LOGGER.info(
		"2. Trigger CDL upgrade device to latest stable build using mock XCONF with factory reset immediately true");
	LOGGER.info("3. Verify factoryResetImmediately log message in Xconf log file");
	LOGGER.info("4. Wait for device to reboot and verify it comes up with requested image");
	LOGGER.info("5. Verify last reboot reason as factory-rest using webpa");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the current image name from device";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get current Image name using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa get command: Device.DeviceInfo.X_CISCO_COM_FirmwareName");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should get the current image name using webpa");
	    LOGGER.info("**********************************************************************************");

	    currentImage = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    status = CommonMethods.isNotNull(currentImage);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully received the current build name as : "
			+ currentImage);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 2
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Unable to get latest image for current box model";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Trigger CDL upgrade device to latest stable build using mock XCONF with factory reset immediately true");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Configure mock server with latest stable image for estb mac of the device"
		    + " and trigger Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow as true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Successfully triggered image download to latest build");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		BroadBandCommonUtils.getAtomSyncUptimeStatus(device, tapEnv);
	    }
	    buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
	    try {
		buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
	    } catch (Exception e) {
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

	    }
	    latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
	    if (CommonMethods.isNull(latestImage)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImage);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);

	    if (CommonMethods.isNotNull(latestImage)) {
		errorMessage = "Unable to trigger CDL latest build - " + latestImage;
		BroadBandXconfCdlUtils.updateSoftwareUpdateConfigurationOnClient(tapEnv, device);
		XConfUtils.configureXconfDownloadFirmwareDetails(device, latestImage, false,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP, (String) null,
			XConfUtils.getFirmwareLocation(BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP, device,
				latestImage),
			BroadBandTestConstants.CONSTANT_0, true);

		isCdlDataPosted = true;

		if (!DeviceModeHandler.isDSLDevice(device)) {
		    BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		} else {
		    FirmwareDownloadUtils.triggerCdlUsingShellScriptForDSL(tapEnv, device);
		}
		status = true;
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully triggered image download to latest stable build");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    // step 3
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the log message factory reset immediately as true in xconf.txt.0 log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify factoryResetImmediately log message in Xconf log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"factoryResetImmediately : TRUE\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Successfully verified factory reset immediately flag in xconf log file");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCdlConstants.LOG_MESSAGE_FACTORY_RESET_IMMEDIATELY_TRUE,
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified factory reset immediately as true in xconf.txt.0 log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 4
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to access the device after reboot or device is not came up with requested build";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Wait for device to reboot and verify it comes up with requested image");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute commands: echo test_connection , cat /version.txt");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device came up successfully after reboot with requested image");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_8)) {
		errorMessage = "Device not accessible after reboot for software upgrade";
		if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
		    errorMessage = "Image did not change after reboot for software upgrade";
		    status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device, latestImage);
		}
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the latest build name in device after CDL");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 5
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the response for webpa parameter or last reboot reason is not factory-reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify last reboot reason as factory-rest using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa get command: Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Should get the last reboot reason as factory reset from webpa");
	    LOGGER.info("**********************************************************************************");

	    BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Last reboot reason is not factory-reset";
		status = response.equalsIgnoreCase(BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET);
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the last reboot reason as factory-reset using webpa");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    /**
	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
	     */
	    if (isCdlDataPosted) {
		postConStepNumber++;
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
			postConStepNumber);
	    }
	    String imageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

	    if (CommonMethods.isNotNull(currentImage) && CommonMethods.isNotNull(imageName)
		    && !(currentImage.equalsIgnoreCase(imageName))) {
		status = false;
		errorMessage = "Unable to revert image back to original build";
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 2 : DESCRIPTION : 1. Revert image to original build if required");
		LOGGER.info("POST-CONDITION 2 : ACTION : Execute webpa or dmcli command to set value of "
			+ "ManageableNotificationEnable parameter to true, Revert image if required");
		LOGGER.info("POST-CONDITION 2 : EXPECTED : Post condition completed successfully");
		LOGGER.info("#######################################################################################");
		status = FirmwareDownloadUtils.triggerCdlUsingTr181OrTftp(tapEnv, device, currentImage);

		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
		}

		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_3);

		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-WAREHOUSE-1001");
    }

    /**
     * Verify warehouse staging using webpa method
     * <ol>
     * <li>1. Get current Image name using webpa</li>
     * <li>2. Get the latest stable build and configure firmware download url using webpa</li>
     * <li>3. Verify firmware download url log message in CM log file</li>
     * <li>4. Configure Firmware name using wbepa</li>
     * <li>5. Verify firmware name in CM log file</li>
     * <li>6. Configure Firmware download and factory reset using webpa</li>
     * <li>7. Verify firmwareDownloadAndFactoryReset log message in PAM log file</li>
     * <li>8. Wait for device to reboot and verify it comes up with requested image</li>
     * <li>9. Verify last reboot reason as factory-rest using webpa</li>
     * <li>10. Verify default value of firmwareDownloadAndFactoryReset parameter value</li>
     * </ol>
     * 
     * @author ArunKumar Jayachandran
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WAREHOUSE-1002")
    public void testVerifyWarehouseResetUsingWebpa(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WAREHOUSE-102";
	String stepNum = "s1";
	String response = null;
	String errorMessage = null;
	boolean status = false;
	String currentImage = null;
	String latestImage = null;
	String logFileCmOrEponAgent = null;
	String codeDownloadUrl = null;
	int stepNumber = BroadBandTestConstants.CONSTANT_1;

	String buildExtension = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WAREHOUSE-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Verify firmware download event notifications for invalid build with Manageable notification feature enabled");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Get current Image name using webpa");
	LOGGER.info("2. Get the latest stable build and configure firmware download url using webpa");
	LOGGER.info("3. Verify firmware download url log message in CM log file");
	LOGGER.info("4. Configure Firmware name using wbepa");
	LOGGER.info("5. Verify firmware name in CM log file");
	LOGGER.info("6. Configure Firmware download and factory reset using webpa");
	LOGGER.info("7. Verify firmwareDownloadAndFactoryReset log message in PAM log file");
	LOGGER.info("8. Wait for device to reboot and verify it comes up with requested image");
	LOGGER.info("9. Verify last reboot reason as factory-rest using webpa");
	LOGGER.info("10. Verify default value of firmwareDownloadAndFactoryReset parameter value");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the current image name from device";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get current Image name using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa get command: Device.DeviceInfo.X_CISCO_COM_FirmwareName");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should get the current image name using webpa");
	    LOGGER.info("**********************************************************************************");

	    currentImage = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    status = CommonMethods.isNotNull(currentImage);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully received the current build name as : "
			+ currentImage);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 2
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to configure the firmware download Url using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Get the latest stable build and configure firmware download url using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: 1. Get the latest stable build 2. Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadURL string "+codeDownloadUrl);
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa set should be successful");
	    LOGGER.info("**********************************************************************************");

	    codeDownloadUrl = BroadbandPropertyFileHandler.getCodeDownloadUrl();

	    buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
	    try {
		buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
	    } catch (Exception e) {
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

	    }
	    latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
	    if (CommonMethods.isNull(latestImage)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImage);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);

	    if (CommonMethods.isNotNull(latestImage)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandCdlConstants.WEBPA_PARAM_CODE_DOWNLOAD_URL, BroadBandTestConstants.CONSTANT_0,
			codeDownloadUrl);
	    }

	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL : Successfully configured firmware download URL using webpa");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 3
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the log message in CM/EPONAgent log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify firmware download url log message in CM/EPONAgent log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"URL is "+codeDownloadUrl+" /rdklogs/logs/Cmlog.txt.0");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should get the log message in CM log file");
	    LOGGER.info("**********************************************************************************");

	    logFileCmOrEponAgent = DeviceModeHandler.isFibreDevice(device) ? BroadBandCdlConstants.EPONAGENT_LOG
		    : BroadBandCdlConstants.CM_LOG_TXT_0;
	    if (CommonMethods.isNotNull(logFileCmOrEponAgent)) {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandCdlConstants.LOG_MESSAGE_CDL_URL + codeDownloadUrl, logFileCmOrEponAgent,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the firmware download url in CM/EPONAgent log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 4
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to configure the firmware name using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Configure Firmware name using wbepa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareToDownload string <latest build name>");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : webpa set should be successful");
	    LOGGER.info("**********************************************************************************");

	    try {
		if (!tapEnv.isImageAvailableInCDL(device, null, latestImage)) {
		    LOGGER.error("REQUEST IMAGE VERSION: " + latestImage
			    + " IS NOT AVAILABLE IN DAC 15/ PROD CDL SERVERS. HENCE BLOCKING THE EXECUTION.");
		}
	    } catch (Exception var12) {
		LOGGER.error("EXCEPTION OCCURRED: " + var12.getMessage());
	    }

	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandCdlConstants.WEBPA_PARAM_CODE_DOWNLOAD_IMAGE_NAME, BroadBandCommonUtils
			    .concatStringUsingStringBuffer(latestImage, BroadBandCdlConstants.BIN_EXTENSION),
		    BroadBandTestConstants.CONSTANT_0);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully configured firmware name using webpa set operation");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 5
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the log message in CM/EPONAgent log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify firmware name in CM/EPONAgent log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"Factory reset image :\" /rdklogs/logs/Cmlog.txt.0 or EPONAGENTlog.txt.0");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : Should get the image name and url in CM//EPONAgent log file");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCdlConstants.LOG_MESSAGE_FACTORY_RESET_IMAGE, logFileCmOrEponAgent,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, latestImage);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the factory reset image log message in CM/EPONAgent log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 6
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to configure FirmwareDownloadAndFactoryReset using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Configure Firmware download and factory reset using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadAndFactoryReset int 1");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa set should be successful ");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		BroadBandCommonUtils.getAtomSyncUptimeStatus(device, tapEnv);
	    }
	    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandCdlConstants.WEBPA_PARAM_CDL_AND_FACTORYRESET,
		    BroadBandTestConstants.STRING_CONSTANT_1, BroadBandTestConstants.CONSTANT_1);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully initiated firmware download and factory reset using webpa");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 7
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the log message in PAM log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify firmwareDownloadAndFactoryReset log message in PAM log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"FirmwareDownloadAndFactoryReset\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should get the log message in PAM log file");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCdlConstants.LOG_MESSAGE_CDL_AND_FACTORY_RESET,
		    BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, latestImage)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, codeDownloadUrl);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified log message FirmwareDownloadAndFactoryReset in PAM log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 8
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to access the device after reboot or device is not came up with requested build";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Wait for device to reboot and verify it comes up with requested image");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute commands: echo test_connection , cat /version.txt");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device came up successfully after reboot with requested image");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_8)) {
		errorMessage = "Device not accessible after reboot for software upgrade";
		if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
		    errorMessage = "Image did not change after reboot for software upgrade";
		    status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device, latestImage);
		}
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the latest build name in device after CDL");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 9
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the response for webpa parameter or last reboot reason is not factory-reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify last reboot reason as factory-rest using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa get command: Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Should get the last reboot reason as factory reset from webpa");
	    LOGGER.info("**********************************************************************************");

	    BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Last reboot reason is not factory-reset";
		status = response.equalsIgnoreCase(BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET);
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the last reboot reason as factory-reset using webpa");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	    // step 10
	    stepNum = "s" + ++stepNumber;
	    errorMessage = "Failed to get the default for firmwareDownloadAndFactoryReset as 0 using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify default value of firmwareDownloadAndFactoryReset parameter value");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa get command: Device.DeviceInfo.X_RDKCENTRAL-COM_FirmwareDownloadAndFactoryReset");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should get the default value as 0 using webpa");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandCdlConstants.WEBPA_PARAM_CDL_AND_FACTORYRESET, BroadBandTestConstants.STRING_ZERO,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the default value of firmwareDownloadAndFactoryReset parameter as 0");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##############################################################################################//

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    /**
	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
	     */
	    String imageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

	    if (CommonMethods.isNotNull(currentImage) && CommonMethods.isNotNull(imageName)
		    && !(currentImage.equalsIgnoreCase(imageName))) {
		status = false;
		errorMessage = "Unable to revert image back to original build";
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 1 : DESCRIPTION : 1. Revert image to original build if required");
		LOGGER.info("POST-CONDITION 1 : ACTION : Execute webpa or dmcli command to set value of "
			+ "ManageableNotificationEnable parameter to true, Revert image if required");
		LOGGER.info("POST-CONDITION 1 : EXPECTED : Post condition completed successfully");
		LOGGER.info("#######################################################################################");
		status = FirmwareDownloadUtils.triggerCdlUsingTr181OrTftp(tapEnv, device, currentImage);

		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
		}
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_2);

		/**
		 * POST-CONDITION 3 : ENABLE THE PUBLIC WIFI
		 */
		BroadBandPostConditionUtils.executePostConditionToEnablePublicWifi(device, tapEnv,
			BroadBandTestConstants.CONSTANT_3);

		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WAREHOUSE-1002");
    }
}
