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
package com.automatics.rdkb.bluetooth;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.CommonUtils;

public class BroadbandBluetoothTest extends AutomaticsTestBase{

    /**
     * Limit Bluetooth LE beacon detection
     * <ol>
     * 
     * <li>set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio to true</li>
     * <li>set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to true</li>
     * <li>Set LimitBeaconDetection to true</li>
     * <li>restart ble.service and Verify status of ble.service shows the message "Started Ble service"</li>
     * <li>Verify log message "Tile Discovery disabled or beacon detection is limited" from
     * /rdklogs/logs/Blelog.txt.0</li>
     * <li>Reboot the device</li>
     * <li>Verify LimitBeaconDetection is still true after reboot</li>
     * <li>Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio is still true</li>
     * <li>Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery is still true</li>
     * <li>Set LimitBeaconDetection to false</li>
     * <li>restart ble.service and Verify status of ble.service shows the message "Started Ble service"</li>
     * <li>Verify log message "Started Bluetooth LE advertisement scan" or "Boot packet received: gecko_evt_system_boot"
     * from /rdklogs/logs/Blelog.txt.0</li>
     * <li>set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to FALSE</li>
     * <li>Set LimitBeaconDetection to true</li>
     * <li>restart ble.service and Verify status of ble.service shows the message "Started Ble service"</li>
     * <li>Verify log message should not display "Tile Discovery disabled or beacon detection is limited" from
     * /rdklogs/logs/Blelog.txt.0</li>
     * <li>Set LimitBeaconDetection to false</li>
     * <li>restart ble.service and Verify status of ble.service shows the message "Started Ble service"</li>
     * <li>Verify log message should not display "Started Bluetooth LE advertisement scan" or "Boot packet received:
     * gecko_evt_system_boot" from /rdklogs/logs/Blelog.txt.0</li>
     * 
     * @author Dipankar Nalui
	 * @refactor Athira
     * 
     * 
     * </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-LIMIT_BLUETOOTH-1000")
    public void testToVerifyLimitBluetoothBeaconDetection(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-LIMIT_BLUETOOTH-100";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	boolean isBleRadioEnabled = false;
	boolean isBleDiscoveryEnabled = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-LIMIT_BLUETOOTH-1000");
	LOGGER.info("TEST DESCRIPTION: Limit Bluetooth LE beacon detection");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio to true");
	LOGGER.info("2. set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to true");
	LOGGER.info("3. Set LimitBeaconDetection to true");
	LOGGER.info(
		"4. restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	LOGGER.info(
		"5. Verify log message \"Tile Discovery disabled or beacon detection is limited\" from /rdklogs/logs/Blelog.txt.0");
	LOGGER.info("6. Reboot the device");
	LOGGER.info("7. Verify LimitBeaconDetection is still true after reboot");
	LOGGER.info("8. Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio is still true");
	LOGGER.info("9. Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery is still true");
	LOGGER.info("10. Set LimitBeaconDetection to false");
	LOGGER.info(
		"11. restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	LOGGER.info(
		"12. Verify log message \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" from /rdklogs/logs/Blelog.txt.0");
	LOGGER.info("13. set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to FALSE");
	LOGGER.info("14. Set LimitBeaconDetection to true");
	LOGGER.info(
		"15. restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	LOGGER.info(
		"16. Verify log message should not display \"Tile Discovery disabled or beacon detection is limited\" from /rdklogs/logs/Blelog.txt.0");
	LOGGER.info("17. Set LimitBeaconDetection to false");
	LOGGER.info(
		"18. restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	LOGGER.info(
		"19. Verify log message should not display \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" from /rdklogs/logs/Blelog.txt.0");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio was not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio to true");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio to true");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio should be set to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_RADIO, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio was set to true");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery was not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to true");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the following command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to true");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery should be set to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_DISCOVERY, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery was set to true");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "LimitBeaconDetection was not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Set LimitBeaconDetection to true");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the command to set Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection to true");
	    LOGGER.info("STEP 3: EXPECTED : LimitBeaconDetection should be set to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIMIT_BEACON_DETECTION, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : LimitBeaconDetection was set to true");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "ble.service was not restarted. Status of ble.service did not show the message \"Started Ble service\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command: systemctl restart ble.service;systemctl status ble.service");
	    LOGGER.info(
		    "STEP 4: EXPECTED : ble.service should be restarted. Status of ble.service should show the message \"Started Ble service\"");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_RESTART_BLE_SERVICE);
	    response = tapEnv.executeCommandInSettopBox(device,
		    BroadBandCommandConstants.CMD_TO_VERIFY_BLE_SERVICE_STARTED);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : ble.service was restarted. Status of ble.service showed the message \"Started Ble service\"");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Log message did not show \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify log message \"Tile Discovery disabled or beacon detection is limited\" from /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:grep -i \"Tile Discovery disabled or beacon detection is limited\" /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Log message should show \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info("**********************************************************************************");

	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_BLUETOOTH_BEACON_DETECTION,
		    BroadBandTestConstants.FILE_BLE_LOG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Log message showed \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Reboot failed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 6: ACTION : Execute the following command:/sbin/reboot");
	    LOGGER.info("STEP 6: EXPECTED : Reboot should be successful");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    errorMessage = status ? "Device rebooted successfully " : "Failed to reboot and access the device";

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Reboot was successful");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "LimitBeaconDetection did not persist as true after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify LimitBeaconDetection is still true after reboot");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute command to verify Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection is true");
	    LOGGER.info("STEP 7: EXPECTED : LimitBeaconDetection should persists as true after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIMIT_BEACON_DETECTION, BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : LimitBeaconDetection persists as true after reboot");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio did not persist as true after reboot";
	    String successMessage = "Successfully verified Webpa param BLERadio is enabled by default";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio is still true");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command to verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio is true");
	    LOGGER.info(
		    "STEP 8: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLERadio should persists as true after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_RADIO, BroadBandTestConstants.TRUE);
	    isBleRadioEnabled = status;
	    if (!status) {
		errorMessage = "Failed to validate BLE Radio parameter via RFC";
		successMessage = "Successfully validated BLE radio parameter via RFC";

		status = BroadBandCommonUtils.verifyFeatureEnableViaRFC(device, tapEnv,
			BroadBandTestConstants.PATTERN_GET_BLE_RADIO_PARAMETER_FROM_RFC_CONFIG,
			BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_RADIO, BroadBandTestConstants.FALSE);
		if (status) {
		    isBleRadioEnabled = BroadBandTestConstants.BOOLEAN_VALUE_FALSE;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery did not persist as true after reboot";
	    successMessage = "Successfully verified Webpa param BLEDiscovery is enabled by default";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery is still true");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the command to verfy Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery is true");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery should persists as true after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_DISCOVERY, BroadBandTestConstants.TRUE);
	    isBleDiscoveryEnabled = status;

	    if (!status) {
		errorMessage = "Step 9: Failed to validate BLE Radio parameter via RFC";
		successMessage = "Step 9: Successfully validated BLE radio parameter via RFC";

		status = BroadBandCommonUtils.verifyFeatureEnableViaRFC(device, tapEnv,
			BroadBandTestConstants.PATTERN_GET_WEBPA_PARAM_BLE_DISCOVERY_FROM_RFC_CONFIG,
			BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_DISCOVERY, BroadBandTestConstants.FALSE);
		if (status) {
		    isBleDiscoveryEnabled = BroadBandTestConstants.BOOLEAN_VALUE_FALSE;
		}
	    }

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "LimitBeaconDetection was not set to false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Set LimitBeaconDetection to false");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute command to set Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection to false");
	    LOGGER.info("STEP 10: EXPECTED : LimitBeaconDetection should be set to false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIMIT_BEACON_DETECTION, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : LimitBeaconDetection was set to false");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "ble.service was not restarted. Status of ble.service did not show the message \"Started Ble service\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the following command: systemctl restart ble.service;systemctl status ble.service");
	    LOGGER.info(
		    "STEP 11: EXPECTED : ble.service should be restarted. Status of ble.service should show the message \"Started Ble service\"");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_RESTART_BLE_SERVICE);
	    response = tapEnv.executeCommandInSettopBox(device,
		    BroadBandCommandConstants.CMD_TO_VERIFY_BLE_SERVICE_STARTED);
	    LOGGER.info("Verify BLE SERVICE STARTED Response = " + response);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : ble.service was restarted. Status of ble.service showed the message \"Started Ble service\"");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Log message did not show \"Started Bluetooth LE advertisement scan\"  or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify log message \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" from /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the following command:grep -E \"Started Bluetooth LE advertisement scan|Boot packet received: gecko_evt_system_boot\" /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Log message should show \"Started Bluetooth LE advertisement scan\"  or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0");
	    if (isBleRadioEnabled && isBleDiscoveryEnabled) {
		status = CommonUtils.searchLogFiles(tapEnv, device,
			BroadBandCommandConstants.CMD_TO_VERIFY_LIMIT_BEACON_DETECTION_SET_TO_FALSE);
		if (status) {
		    LOGGER.info(
			    "STEP 12: ACTUAL : Log message showed \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0");
		} else {
		    LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		if (isBleRadioEnabled) {
		    errorMessage = "Test step not applicable when BLEDiscovery is disabled ";
		} else {
		    errorMessage = "Test step not applicable when BLERadio is disabled ";
		}
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    
	    stepNum = "s13";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery was not set to FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to FALSE");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery to false");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery should be set to FALSE");
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("Clearing the content of the log file /rdklogs/logs/Blelog.txt.0");
	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_CLEAR_BLE_LOG);

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_BLE_DISCOVERY, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BLE.Discovery was set to FALSE");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "LimitBeaconDetection was not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Set LimitBeaconDetection to true");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute command to set Device.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection to true");
	    LOGGER.info("STEP 14: EXPECTED : LimitBeaconDetection should be set to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIMIT_BEACON_DETECTION, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : LimitBeaconDetection was set to true");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "ble.service was not restarted. Status of ble.service did not show the message \"Started Ble service\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute the following command: systemctl restart ble.service;systemctl status ble.service");
	    LOGGER.info(
		    "STEP 15: EXPECTED : ble.service should be restarted. Status of ble.service should show the message \"Started Ble service\"");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_RESTART_BLE_SERVICE);
	    response = tapEnv.executeCommandInSettopBox(device,
		    BroadBandCommandConstants.CMD_TO_VERIFY_BLE_SERVICE_STARTED);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : ble.service was restarted. Status of ble.service showed the message \"Started Ble service\"");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s16";
	    errorMessage = "Log message did not show \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION : Verify log message displays \"Tile Discovery disabled or beacon detection is limited\" from /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute the following command:grep -i \"Tile Discovery disabled or beacon detection is limited\" /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 16: EXPECTED : Log message should show \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info("**********************************************************************************");

	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_BLUETOOTH_BEACON_DETECTION,
		    BroadBandTestConstants.FILE_BLE_LOG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 16: ACTUAL : Log message showed \"Tile Discovery disabled or beacon detection is limited\" in /rdklogs/logs/Blelog.txt.0");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s17";
	    errorMessage = "LimitBeaconDetection was not set to false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Set LimitBeaconDetection to false");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute command to setDevice.DeviceInfo.X_RDKCENTRAL-COM_xBlueTooth.LimitBeaconDetection to false");
	    LOGGER.info("STEP 17: EXPECTED : LimitBeaconDetection should be set to false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_LIMIT_BEACON_DETECTION, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : LimitBeaconDetection was set to false");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");
	    stepNum = "s18";
	    errorMessage = "ble.service was not restarted. Status of ble.service did not show the message \"Started Ble service\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION : restart ble.service and Verify status of ble.service shows the message \"Started Ble service\"");
	    LOGGER.info(
		    "STEP 18: ACTION : Execute the following command: systemctl restart ble.service;systemctl status ble.service");
	    LOGGER.info(
		    "STEP 18: EXPECTED : ble.service should be restarted. Status of ble.service should show the message \"Started Ble service\"");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_RESTART_BLE_SERVICE);
	    response = tapEnv.executeCommandInSettopBox(device,
		    BroadBandCommandConstants.CMD_TO_VERIFY_BLE_SERVICE_STARTED);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info(
			"STEP 18: ACTUAL : ble.service was restarted. Status of ble.service showed the message \"Started Ble service\"");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s19";
	    errorMessage = "Log message showed \"Started Bluetooth LE advertisement scan\"  or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 19: DESCRIPTION : Verify log message should not show  \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" from /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute the following command:grep -E \"Started Bluetooth LE advertisement scan|Boot packet received: gecko_evt_system_boot\" /rdklogs/logs/Blelog.txt.0");
	    LOGGER.info(
		    "STEP 19: EXPECTED : Log message should not show \"Started Bluetooth LE advertisement scan\"  or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0");

	    status = !CommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCommandConstants.CMD_TO_VERIFY_LIMIT_BEACON_DETECTION_SET_TO_FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 19: ACTUAL : Log message did not show \"Started Bluetooth LE advertisement scan\" or \"Boot packet received: gecko_evt_system_boot\" in /rdklogs/logs/Blelog.txt.0");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-LIMIT_BLUETOOTH-1000");
    }
}
