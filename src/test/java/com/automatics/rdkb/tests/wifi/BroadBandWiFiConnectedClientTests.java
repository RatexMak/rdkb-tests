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

package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWiFiConnectedClientTests extends AutomaticsTestBase{
	
	/**
     * Device device MUST have the ability to change from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only radio
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Check the operating mode for 5GHz Wi-Fi from RDKB device</li>
     * <li>Step 2: Check the mode of operation in 5 Ghz client is 802.11a/n/ac</li>
     * <li>Step 3: Change the mode of operation in 5GHz client from 802.11a/n/ac to 802.11ac and verify the status</li>
     * <li>Step 4: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 5: Check the mode of operation in 5GHz client is 802.11ac</li>
     * <li>Step 6: Change the mode of operation in 5 Ghz client to 802.11n</li>
     * <li>Step 7: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 8: Check the mode of operation in 5Ghz client is 802.11n</li>
     * <li>Step 9: Change the mode of operation in 5GHz client to 802.11 n/ac</li>
     * <li>Step 10: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 11: Check the mode of operation in 5Ghz client is 802.11n/ac</li>
     * <li>STEP 12: Verify the mode of operation in 5 Ghz client"</li>
     * </ol>
     * 
     * @author anandam.s
     * @refactor yamini.s
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-3003")

    public void testVerifyChangeOperatingModeFor5GHz(Dut device) {

	// string variable to store test case id
	String testCaseId = "TC-RDKB-WIFI-WEBPA-303";
	// Boolean variable to store the status of each step
	boolean status = false;
	// String variable to store the errorMessage in each step
	String errorMessage = null;
	// String variable to store the successMessage
	String successMessage = null;
	// String variable to store the step number
	String step = "s1";
	// variable to store the instance of connected client
	Dut connectedClientDevice = null;
	// variable to store the ostype of connected client
	String clientOsType = null;
	WifiOperatingStandard defaultOperatingStandard = null;
	BroadBandResultObject resultObject = null;
	try {

	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-3003");

	    LOGGER.info("**************************************************************");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the ability to change mode from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only radio on the 5 GHz radio");

	    LOGGER.info("*************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Verify default operating mode from gateway device for 5 GHz wifi network");
	    LOGGER.info("STEP 2: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 3: Verify default operating mode from client device for 5 GHz wifi network");
	    LOGGER.info("STEP 4: Change the operating mode to 802.11ac");
	    LOGGER.info("STEP 5: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 6: Verify the mode of operation in 5 Ghz client ");
	    LOGGER.info("STEP 7: Change the operating mode to 802.11n in 5Ghz client ");
	    LOGGER.info("STEP 8: Verify 5 Ghz client is connected and IPV4 address obtained ");
	    LOGGER.info("STEP 9: Verify the mode of operation in 5 Ghz client ");
	    LOGGER.info("STEP 10: Change the operating mode to 802.11n/ac in 5Ghz client ");
	    LOGGER.info("STEP 11: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 12: Verify the mode of operation in 5 Ghz client");
	    LOGGER.info("**********************************************************************************");

	    /**
	     * Step1 : Verify the operating mode for 5GHz WiFi from RDKB device
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION :Verify default operating mode from gateway device for 5 GHz wifi network");
	    LOGGER.info("STEP 1: ACTION : Execute command to verify default operating mode 5 Ghz mode of operation");
	    LOGGER.info(
		    "STEP 1: EXPECTED :Operating mode should be 802.11a/n for atom console devices, n for DSL devices and for rest of the devices expected if 802.11a/n/ac ");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N;
	    } else if (BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX;
	    } else {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N_AC;
	    }
	    errorMessage = "Received Operating standard is Not as Expected "
		    + defaultOperatingStandard.getOperatingmode();
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
			defaultOperatingStandard.getOperatingmode());
	    } catch (TestException exception) {
		errorMessage = errorMessage + exception.getMessage();
		LOGGER.info(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL :The operating mode for 5GHz network in RDKB device is as expected ");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step2 : Check 5 GHz client is connected and IPV4 address obtained
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 2: ACTION : Execute command to connect 5 Ghz and get IPV4 address");
	    LOGGER.info("STEP 2: EXPECTED :IPV4 address should be obtained");
	    LOGGER.info("**********************************************************************************");
	    step = "s2";
	    status = false;
	    errorMessage = "Client device is NOT connected properly over WiFi with default operating standard - 802.11a/n/ac";

	    try {
		connectedClientDevice = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
		if (connectedClientDevice != null) {
		    clientOsType = ((Device) connectedClientDevice).getOsType();
		    LOGGER.info("Waiting for 30 seconds to connect to device and acquire IP address");
		    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

		    status = BroadBandConnectedClientUtils
			    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
				    connectedClientDevice, tapEnv)
			    && checkConnectivityOfDevice(connectedClientDevice);
		} else {
		    errorMessage = "Failed to get a Connected client device in 5Ghz band";
		    LOGGER.error(errorMessage);
		}
	    } catch (TestException exception) {
		errorMessage = errorMessage + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL :Client device is connected properly over WiFi with default operating standard ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step3 : Verify the operating mode for 5GHz WiFi from client device
	     */

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION :Verify default operating mode from client device for 5 GHz wifi network");
	    LOGGER.info("STEP 3: ACTION : Execute command to verify  default operating mode from 5 Ghz");
	    LOGGER.info("STEP 3: EXPECTED :Operating mode should be 802.11a or 802.11n or 802.11ac");
	    LOGGER.info("**********************************************************************************");

	    step = "s3";
	    status = false;
	    errorMessage = "The default value for operating mode in 2.4GHz network for client device is NOT either 802.11a 802.11n or 802.11ac'";
	    try {
		status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(connectedClientDevice,
			tapEnv, defaultOperatingStandard.getClientOperatingMode());
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :The operating mode for 5GHz network in client device is a/n/ac ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step4 : Change the mode of operation in 5GHz client to 802.11 ac; Change the mode of operation in 5GHz
	     * client to 802.11a/n/ac for devices
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION :Change the operating mode to 802.11ac in gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP 4: ACTION : Execute command to change the operating mode from 5 Ghz");
	    LOGGER.info(
		    "STEP 4: EXPECTED :The operating mode should change to 802.11ac; for model mode should change to a/n/ac ");
	    LOGGER.info("**********************************************************************************");
	    step = "s4";
	    status = false;

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {
		errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? "Failed to change the operating mode for 5GHz network in Gateway from a/n/ac/ax to a/n/ac mode "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			: "Failed to change the operating mode for 5GHz network in Gateway from a/n/ac"
				+ " to ac mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getOperatingmode(),
				WebPaDataTypes.STRING.getValue())
			: BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				WifiOperatingStandard.OPERATING_STANDARD_AC.getOperatingmode(),
				WebPaDataTypes.STRING.getValue());
		successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? "Changed the operating mode for 5GHz network from a/n/ac/ax to a/n/ac "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			: "Changed the operating mode for 5GHz network from a/n/ac to ac "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		if (status) {
		    LOGGER.info("STEP 4: ACTUAL :" + successMessage);
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		LOGGER.info(
			"waiting for two minutes after changing the operating standard for client to change its operating standard");
		tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		// }
		/**
		 * Step5 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 5: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 5: ACTION : Execute command to connect 5 Ghz nd IPV4 address");
		LOGGER.info("STEP 5: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s5";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to ac -";
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {

		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (resultObject.isStatus()) {
			    status = BroadBandConnectedClientUtils
				    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
					    connectedClientDevice, tapEnv)
				    && checkConnectivityOfDevice(connectedClientDevice);
			} else {
			    errorMessage = "Failed to Connect client device after change in operating standard with message -"
				    + resultObject.getErrorMessage();
			    LOGGER.error(errorMessage);
			}
		    } catch (TestException exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 5: ACTUAL : 5GHz client is connected and IPV4 address obtained");
		    } else {
			LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
		}
		/**
		 * Step6 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 6: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 6: ACTION : Execute command to verify mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 6: EXPECTED :The mode of operation should be 802.11ac; for models mode should be a/n/ac");
		LOGGER.info("**********************************************************************************");
		step = "s6";
		status = false;

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    try {

			errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "The operating mode for 5GHz network is not changed from a/n/ac/ax to a/n/ac mode in client device."
				: "The operating mode for 5GHz network is not changed from \'a/n/ac\' to \'ac\' mode in client device.";
			status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getClientOperatingMode())
				: BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_AC.getClientOperatingMode());
			successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "Changed the operating mode for 5GHz network from a/n/ac/ax to a/n/ac "
					+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
				: "Changed the operating mode for 5GHz network from a/n/ac to ac "
					+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 6: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step7 : Change the mode of operation in 5GHz client to 802.11 n; For devices change mode from a/n/ac
		 * to a/n/ac/ax
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 7: DESCRIPTION :Change the operating mode to 802.11n in 5Ghz gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP 7: ACTION : Execute command to change the mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 7: EXPECTED :The mode of operation should be 802.11n; for devices mode should change to a/n/ac/ax");
		LOGGER.info("**********************************************************************************");
		step = "s7";
		status = false;

		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {

		    errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? "Failed to change the operating mode for 5GHz network from a/n/ac to a/n/ac/ax mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			    : "Failed to change the operating mode for 5GHz network from ac to n mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? BroadBandWiFiUtils.setWebPaParams(device,
				    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				    WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX.getOperatingmode(),
				    WebPaDataTypes.STRING.getValue())
			    : BroadBandWiFiUtils.setWebPaParams(device,
				    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				    WifiOperatingStandard.OPERATING_STANDARD_N.getOperatingmode(),
				    WebPaDataTypes.STRING.getValue());
		    successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? "Changed the operating mode for 5GHz network from a/n/ac to a/n/ac/ax using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			    : "Changed the operating mode for 5GHz network from ac to n using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    if (status) {
			LOGGER.info("STEP 7: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		    LOGGER.info(
			    "waiting for two minutes after changing the operating standard for client to change its operating standard");
		    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		}
		/**
		 * Step8 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 8: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 8: ACTION : Execute command to connect to 5 Ghz and get IPV4 address");
		LOGGER.info("STEP 8: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s8";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to n -";
		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {
		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (resultObject.isStatus()) {
			    status = BroadBandConnectedClientUtils
				    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
					    connectedClientDevice, tapEnv)
				    && checkConnectivityOfDevice(connectedClientDevice);
			} else {
			    errorMessage = "Failed to Connected client device after change in operating standard with message -"
				    + resultObject.getErrorMessage();
			    LOGGER.error(errorMessage);
			}
		    } catch (TestException exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info(
				"STEP 8: ACTUAL :Client device is connected properly over WiFi after changing opearting mode in gateway.");
		    } else {
			LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step9 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 9: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 9: ACTION : Execute command to verify mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 9: EXPECTED :The mode of operation should be 802.11n; for devices mode should be a/n/ac/ax");
		LOGGER.info("**********************************************************************************");
		step = "s9";
		status = false;

		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {
		    try {

			errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "The operating mode for 5GHz network is not changed from a/n/ac to a/n/ac/ax mode in client device."
				: "The operating mode for 5GHz network is not changed from \'ac\' to \'n' mode in client device.";
			status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX.getClientOperatingMode())
				: BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_N.getClientOperatingMode());
			successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "Successfully verified the operating mode for 5GHz network as 802.11a/n/ac/ax"
				: "Successfully verified the operating mode for 5GHz network as 802.11n";
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 9: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}

		/**
		 * Step10 : Change the mode of operation in 5GHz client to 802.11 n/ac
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 10: DESCRIPTION :Change the operating mode to 802.11n/ac in 5Ghz gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP 10: ACTION : Execute command to change mode of operation in 5 Ghz");
		LOGGER.info("STEP 10: EXPECTED :The mode of operation should be 802.11n/ac");
		LOGGER.info("**********************************************************************************");
		step = "s10";
		status = false;
		errorMessage = "Failed to change the operating mode for 5GHz network from n to n/ac mode in Gateway device using WebPA - Device.WiFi.Radio.10100.OperatingStandards";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
			    WifiOperatingStandard.OPERATING_STANDARD_N_AC.getOperatingmode(),
			    WebPaDataTypes.STRING.getValue());
		    if (status) {
			LOGGER.info(
				"STEP 10: ACTUAL :Changed the operating mode for 5GHz network from n to n/ac using WebPA - Device.WiFi.Radio.10100.OperatingStandards.");
		    } else {
			LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		    LOGGER.info(
			    "waiting for two minutes after changing the operating standard for client to change its operating standard");
		    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		}
		/**
		 * Step11 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 11: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 11: ACTION : Execute command to connect to 5 Ghz and get IPV4 address");
		LOGGER.info("STEP 11: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s11";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to n/ac -";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {

		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (resultObject.isStatus()) {
			    status = BroadBandConnectedClientUtils
				    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
					    connectedClientDevice, tapEnv)
				    && checkConnectivityOfDevice(connectedClientDevice);
			} else {
			    errorMessage = "Failed to Connected client device after change in operating standard with message -"
				    + resultObject.getErrorMessage();
			    LOGGER.error(errorMessage);
			}
		    } catch (TestException exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info(
				"STEP 11: ACTUAL :Successfully connected to 5Ghz wifi client and verified IPV4 address");
		    } else {
			LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step12 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 12: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 12: ACTION : Execute command to verify the mode of operation in 5 Gh");
		LOGGER.info("STEP 12: EXPECTED :The mode of operation should be 802.11n or 802.11ac");
		LOGGER.info("**********************************************************************************");
		step = "s12";
		status = false;
		errorMessage = "The operating mode for 5GHz network is not changed from \'802.11n\' to \'802.11n or 802.11ac\' mode in client device.";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    try {
			status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
				connectedClientDevice, tapEnv,
				WifiOperatingStandard.OPERATING_STANDARD_N_AC.getClientOperatingMode());
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 12: ACTUAL :Successfully verified operating mode for 5GHz network ");
		    } else {
			LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
	    }
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception occured while changing the operating mode from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only for 5 GHz network using WebPA - Device.WiFi.Radio.10100.OperatingStandards"
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {

	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
		    defaultOperatingStandard.getOperatingmode(), WebPaDataTypes.STRING.getValue());
	    LOGGER.info("Finally : " + (status
		    ? "Changed the operating mode for 5GHz network to default value - a/n/ac using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
		    : "Operating mode is not changed to default value using WebPA - Device.WiFi.Radio.10100.OperatingStandards"));

	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-WEBPA-3003");
    }
    
    /**
     * Method to check the connectivity of device over wlan
     * 
     * @param Dut
     *            device
     * 
     * @return boolean connection status
     * @refactor Alan_Bivera
     */

    private boolean checkConnectivityOfDevice(Dut device) {

	LOGGER.debug("Entering checkConnectivityOfDevice");
	String commandResponse = null;
	String command = ((Device) device).getOsType().equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
		? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS.replace("<INTERFACE>",
				BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
		: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
	commandResponse = tapEnv.executeCommandOnOneIPClients(device, command);
	LOGGER.info("Curl response from device - " + commandResponse);

	if ((CommonMethods.isNotNull(commandResponse) && commandResponse.contains("200 OK"))) {
	    LOGGER.info("Obtained 200 ok message on checking connectivity");
	    LOGGER.debug("Ending heckConnectivityOfDevice");
	    return true;
	} else {
	    throw new TestException("Device failed connectivity test using curl command");
	}
    }
    
    /**
     * 
     * <li>1. Update wifiClient mac address using webpa</li>
     * <li>2. Update reporting period value as 5 using webpa</li>
     * <li>3. Update override TTL value as 900 using webpa</li>
     * <li>4. Create WiFi debug monitor log in nvram to capture the client report</li>
     * <li>5. Update wifiClient enable status as true using webpa</li>
     * <li>6. Get client report from wifiMon file</li>
     * <li>7. Verify single client report log message in wifilog</li>
     * <li>8. Verify cloud url to upload the client report in parodus log file</li>
     * <li>9. Factory Reset using WebPA request using value \"Router\"</li>
     * <li>10. Verify reporting period value set to default after factory reset</li>
     * <li>11. Verify override TTL value set to default after facotry reset</li>
     * <li>12. Verify wifiClient enable status set to default state after factory reset</li>
     * <li>13. Verify wificlient schema using Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Schema after factory
     * reset</li>
     * <li>14. Verify wifiClient mac address reset to default value after factory reset</li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor Said Hisham
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SECURITY })
    @TestDetails(testUID = "TC-RDKB-HARVESTER-REPORT-1002")
    public void testToVerifySingleClientHarvesterFor2GhzClient(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-HARVESTER-REPORT-1002");
	LOGGER.info("TEST DESCRIPTION: Test to verify single client harvester report for 2.4GHz client");
	LOGGER.info("NOTES: This test case is written as part of validating RDKB-24606");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update wifiClient mac address using webpa");
	LOGGER.info("2. Update reporting period value as 5 using webpa");
	LOGGER.info("3. Update override TTL value as 900 using webpa");
	LOGGER.info("4. Create WiFi debug monitor log in nvram to capture the client report");
	LOGGER.info("5. Update wifiClient enable status as true using webpa");
	LOGGER.info("6. Get client report from wifiMon file");
	LOGGER.info("7. Verify single client report log message in wifilog");
	LOGGER.info("8. Verify cloud url to upload the client report in parodus log file");
	LOGGER.info("9. Factory Reset using WebPA request using value \"Router\"");
	LOGGER.info("10. Verify reporting period value set to default after factory reset");
	LOGGER.info("11. Verify override TTL value set to default after facotry reset");
	LOGGER.info("12. Verify wifiClient enable status set to default state after factory reset");
	LOGGER.info(
		"13. Verify wificlient schema using Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Schema after factory reset");
	LOGGER.info("14. Verify wifiClient mac address reset to default value after factory reset");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-HARVESTER-REPORT-102";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	String macAddress = null;
	Dut clientSettop = null;
	boolean preStatus = false;
	// variable declaration ends

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the 2.4GHz WiFi client");
	    LOGGER.info("PRE-CONDITION : ACTION : Connect client with 2.4GHz SSID & Passphrase");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Client should connect with 2.4GHz WiFi");
	    errorMessage = "Failed to connect the client using 2.4GHz WiFi band";
	    clientSettop = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    preStatus = null != clientSettop;
	    if (!preStatus) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "PRE_CONDITION_FAILED: Unable to get 2.4GHz client device");
	    }
	    LOGGER.info("PRE-CONDITION : ACTUAL: Successfully connected to 2.4GHz wifi client device");
	    LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + preStatus);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Update wifiClient mac address using webpa");
	    LOGGER.info(
		    "STEP 1: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress data type: 0 value: <2.4GHz WiFi client mac>");
	    LOGGER.info("STEP 1: EXPECTED: Webpa set operation should be success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress";
	    macAddress = ((Device) clientSettop).getConnectedDeviceInfo().getWifiMacAddress();
	    String formattedMacAddress = macAddress;
	    if (CommonMethods.isNotNull(macAddress)) {
		macAddress = macAddress.replace(BroadBandTestConstants.DELIMITER_COLON,
			BroadBandTestConstants.EMPTY_STRING);
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS, BroadBandTestConstants.CONSTANT_0,
			macAddress);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Successfully updated client wifi mac using webpa");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Update reporting period value as 5 using webpa");
	    LOGGER.info(
		    "STEP 2: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod data type: 2 value: 5");
	    LOGGER.info("STEP 2: EXPECTED: Webpa set operation should be success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the reporting period value using webpa";
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_5);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: Successfully updated wifi client reporting period as 5 using webpa");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Update override TTL value as 900 using webpa");
	    LOGGER.info(
		    "STEP 3: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL data type: 2 value: 900");
	    LOGGER.info("STEP 3: EXPECTED: Webpa set operation should be success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL";
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
		    BroadBandTestConstants.CONSTANT_2, Integer.toString(BroadBandTestConstants.CONSTANT_900));
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully verified wifi client mac address length using webpa");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s4";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Create WiFi debug monitor log in nvram to capture the client report");
	    LOGGER.info("STEP 4: ACTION: Execute command: touch /nvram/wifiMonDbg");
	    LOGGER.info("STEP 4: EXPECTED: Should create a debug file under /nvram directory");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to create the file under /nvram directory";
	    BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
			    BroadBandCommandConstants.FILE_WIFI_MON_DBG));
	    status = (CommonMethods.isAtomSyncAvailable(device, tapEnv)) ? BroadBandCommonUtils
		    .doesFileExistInAtomConsole(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG).isStatus()
		    : CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG);
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL: Successfully created wifi monitoring debug log file under nvram directory");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s5";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION: Update wifiClient enable status as true using webpa");
	    LOGGER.info(
		    "STEP 5: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled  datatype: boolean value: true");
	    LOGGER.info("STEP 5: EXPECTED: Webpa set operation should be success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled as true";
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: Successfully enabled wifi client using webpa");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s6";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Get client report from wifiMon file");
	    LOGGER.info("STEP 6: ACTION: Execute command: grep -i wifi Destination /tmp/wifiMon");
	    LOGGER.info("STEP 6: EXPECTED: Should get the report for configured wifi client");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message wifi destination for configured client";
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_WIFI_DESTINATION, BroadBandCommandConstants.FILE_WIFI_MON,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    // verify single client server url in wifi destination output
	    if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.SINGLE_CLIENT_REPORT_SERVER_URL)) {
		errorMessage = "Failed to get the log message polled station info for configured wifi client";
		response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_POLLED_STATION_INFO,
			BroadBandCommandConstants.FILE_WIFI_MON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		// verify wifi client mac & vap value in polled station info output
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				formattedMacAddress.toLowerCase())
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTestConstants.PRIVATE_VAP_COLON,
					BroadBandTestConstants.STRING_CONSTANT_1));
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Successfully received the harvester logs for wifi client");
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s7";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Verify single client report log message in wifilog");
	    LOGGER.info("STEP 7: ACTION: Execute command: grep -i single client report /rdklogs/logs/WiFilog.txt.0");
	    LOGGER.info("STEP 7: EXPECTED: Response should contain the log message with transaction id");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message in WiFilog file";
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_SINGLE_CLIENT_REPORT,
		    BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL: Successfully verified the single client report log message in wifi log file");
	    } else {
		LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s8";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION: Verify cloud url to upload the client report in parodus log file");
	    LOGGER.info(
		    "STEP 8: ACTION: Execute command: grep -i raw.kestrel.reports.WifiSingleClient /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 8: EXPECTED: Response should contain the cloud url in the PARODUSlog.txt.0 file");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message in PARODUSlog.txt.0 file";
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.SINGLE_CLIENT_REPORT_SERVER_URL, BroadBandCommandConstants.LOG_FILE_PARODUS,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL: Successfully verified single wifi client report cloud url in parodus log file");
	    } else {
		LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s9";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: Factory Reset the device using WebPA request");
	    LOGGER.info(
		    "STEP 9: ACTION: Execute webpa set command: parameter: Device.X_CISCO_COM_DeviceControl.FactoryReset data type: 0 value: 900");
	    LOGGER.info("STEP 9: EXPECTED: Webpa set operation should be success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to factory reset the device or device not coming up after factory reset";
	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL: Successfully factory reseted the device and device came up after factory reset");
	    } else {
		LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s10";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION: Verify reporting period value set to default after factory reset");
	    LOGGER.info(
		    "STEP 10: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod");
	    LOGGER.info("STEP 10: EXPECTED: Should get the response as 0 after factory reset");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for reporting period from webpa or response is not matched with default value";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.STRING_ZERO,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL: Successfully verified wifi client reporting period value is set to 0 after factory reset");
	    } else {
		LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s11";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION: Verify override TTL value set to default after facotry reset");
	    LOGGER.info(
		    "STEP 11: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL");
	    LOGGER.info("STEP 11: EXPECTED: Should get the response as 0 after factory reset");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for override TTL from webpa or response is not matched with default value";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
		    BroadBandTestConstants.STRING_ZERO, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL: Successfully verified wifi client default override TTL value is set to 0 after factory reset");
	    } else {
		LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s12";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION: Verify wifiClient enable status set to default state after factory reset");
	    LOGGER.info(
		    "STEP 12: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled");
	    LOGGER.info("STEP 12: EXPECTED: Should get the response as false after factory reset");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for wifi client enabled status from webpa or response is not matched with default value";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL: Successfully verified wifi client enabled status as false after factory reset");
	    } else {
		LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s13";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION: Verify wificlient schema using Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Schema after factory reset");
	    LOGGER.info(
		    "STEP 13: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Schema");
	    LOGGER.info("STEP 13: EXPECTED: Should get the response as WifiSingleClient.avsc");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for wifi client schema  from webpa or response is not matched with default value";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_SCHEMA,
		    BroadBandTestConstants.SINGLE_CLIENT_REPORT_SCHEMA, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL: Successfully verified wifi client schema value as WifiSingleClient.avsc after factory reset");
	    } else {
		LOGGER.error("STEP 13: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//
	    

	    stepNumber = "s14";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION: Verify wifiClient mac address reset to default value after factory reset");
	    LOGGER.info(
		    "STEP 14: ACTION: Execute webpa get command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress");
	    LOGGER.info("STEP 14: EXPECTED: The value should return 000000000000");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for wifi client mac from webpa or response is not matched with default value";
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS,
		    BroadBandTestConstants.DEFAULT_MAC_ADDRESS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL: Successfully verified wifi client mac address set to default value after factory reset");
	    } else {
		LOGGER.error("STEP 14: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying single client harvester report" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    if (preStatus) {
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_1);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-HARVESTER-REPORT-1002");
	// ###############################################################//
    }
    



}
