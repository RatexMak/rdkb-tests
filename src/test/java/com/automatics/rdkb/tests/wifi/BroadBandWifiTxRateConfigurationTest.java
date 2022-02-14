/**
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

/**
 * Class for Validating the Configuration of Transmission Rates for both 2.4GHz Radio and 5GHz radio on BroadBand
 * Devices.
 */

import org.testng.annotations.Test;
import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;

public class BroadBandWifiTxRateConfigurationTest extends AutomaticsTestBase {
    
    /** Instance variable to store the Factory Reset Flag value */
    private boolean performedFactoryResetInitTest = false;

    /**
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>STEP 1: Set channel to 153</li>
     * <li>STEP 2: Apply radio settings</li>
     * <li>STEP 3: Check the Channel</li>
     * <li>STEP 4: Check the power limit in 153 channel</li>
     * <li>STEP 5: To check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40</li>
     * <li>STEP 6:Apply radio settings</li>
     * <li>STEP 7: Check the Channel</li>
     * <li>STEP 8: Check the power limit in 40 channel</li>
     * <li>STEP 9: To check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36</li>
     * <li>STEP 10:Apply radio settings</li>
     * <li>STEP 11 Check the Channel</li>
     * <li>STEP 12: Check the power limit in 36 channel</li>
     * <li>STEP 13: To check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48</li>
     * <li>STEP 14:Apply radio settings</li>
     * <li>STEP 15: Check the Channel</li>
     * <li>STEP 16: Check the power limit in 48 channel</li>
     * </ol>
     * 
     * @author Deepa Bada
     * @refactor Athira
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-TX-RATE-CONFIG-5064", testDecription = "Verify default SSID using WebPA ")
    public void testHighPowerSupportOn5GBandChannels(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-TX-RATE-CONFIG-564";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String channelValue = null;
	String meshInitialStatus = null;
	String response = null;
	int stepCounter = BroadBandTestConstants.CONSTANT_0;
	try {
	    LOGGER.info("************************* STARTING PRE-CONFIGURATIONS *************************");
	    meshInitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    BroadBandPreConditionUtils.executePreConditionToDisableMeshUsingSystemCommands(device, tapEnv,
		    meshInitialStatus, BroadBandTestConstants.CONSTANT_1);
	    ;
	    LOGGER.info("************************* COMPLETED PRE-CONFIGURATIONS *************************");

	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TX-RATE-CONFIG-564");
	    LOGGER.info("TEST DESCRIPTION: To verfiy high power support in 5GHZ band channel");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("STEP 1: Set channel to 153");
	    LOGGER.info("STEP 2: Apply radio settings");
	    LOGGER.info("STEP 3: Check the Channel");
	    LOGGER.info("STEP 4: Check the power limit in 153 channel");
	    LOGGER.info("STEP 5: To check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40");
	    LOGGER.info("STEP 6:Apply radio settings");
	    LOGGER.info("STEP 7: Check the Channel");
	    LOGGER.info("STEP 8: Check the power limit in 40 channel");
	    LOGGER.info("STEP 9: To check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36");
	    LOGGER.info("STEP 10:Apply radio settings");
	    LOGGER.info("STEP 11: Check the Channel");
	    LOGGER.info("STEP 12: Check the power limit in 36 channel");
	    LOGGER.info("STEP 13: To check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48");
	    LOGGER.info("STEP 14:Apply radio settings");
	    LOGGER.info("STEP 15: Check the Channel");
	    LOGGER.info("STEP 16: Check the power limit in 48 channel");

	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Set channel to 153 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : check the current channel and Set channel to 153 ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 153 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");
	    channelValue = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    if (CommonMethods.isNotNull(channelValue)) {
		if (!channelValue.equalsIgnoreCase(BroadBandTestConstants.CONSTANT_153)) {
		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
			    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_153);
		} else
		    status = true;
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 153 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;
	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    WebPaParameter webPaParameter = new WebPaParameter();
	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue(BroadBandTestConstants.TRUE);
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the Channel  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
	    LOGGER.info(
		    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 153");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.CONSTANT_153);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the power limit in 153 channel ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 153 channel ");
	    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
	    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
	    if (CommonMethods.isNotNull(response)) {
		int value = Integer.parseInt(response);
		if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
		    status = true;

	    }
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter
		    + ": DESCRIPTION : To check power limit at mid range of UNII-1 channels.Set UNII-1 channel to 40");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 40 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_FORTY);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 40 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue(BroadBandTestConstants.TRUE);
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the Channel  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
	    LOGGER.info(
		    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 40");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.STRING_VALUE_FORTY);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : channel verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the power limit in 40 channel ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 40 channel ");
	    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
	    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
	    if (CommonMethods.isNotNull(response)) {
		int value = Integer.parseInt(response);
		if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
		    status = true;
	    }
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter
		    + ": DESCRIPTION : To check power limit at low range of UNII-1 channels.Set UNII-1 channel to 36");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 36 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.RADIO_CHANNEL_36);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to 36 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue(BroadBandTestConstants.TRUE);
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the Channel  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
	    LOGGER.info(
		    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 36");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.RADIO_CHANNEL_36);

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the power limit in 36 channel ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 36 channel ");
	    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
	    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
	    if (CommonMethods.isNotNull(response)) {
		int value = Integer.parseInt(response);
		if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
		    status = true;

	    }

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL :  power limit verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48 ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter
		    + ": DESCRIPTION : To check power limit at high range of UNII-1 channels.Set UNII-1 channel to 48");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.Channel uint 36 ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :set is successful");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.RADIO_CHANNEL_48);
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel set to  48 Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Apply radio settings  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Apply radio settings ");
	    LOGGER.info("STEP " + stepCounter
		    + ": ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");

	    webPaParameter.setName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING);
	    webPaParameter.setValue("true");
	    webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
	    status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Radio settings applied Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the Channel  ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the Channel  ");
	    LOGGER.info(
		    "STEP " + stepCounter + ": ACTION :Execute command :dmcli eRT getv Device.WiFi.Radio.2.Channel ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :Channel is 48");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
	    status = CommonMethods.isNotNull(response) && response.contains(BroadBandTestConstants.RADIO_CHANNEL_48);
	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : Channel verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    stepCounter += BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepCounter;

	    errorMessage = "Unable to Check the power limit in 36 channel ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCounter + ": DESCRIPTION : Check the power limit in 36 channel ");
	    LOGGER.info("STEP " + stepCounter + ": ACTION :Execute command :iwlist ath1 txpower ");
	    LOGGER.info("STEP " + stepCounter + ": EXPECTED :power range is 25 -30dbm");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TXPOWER);
	    response = CommonMethods.patternFinder(response, BroadBandCommandConstants.PATTERN_TO_FIND_POWERLIMIT);
	    if (CommonMethods.isNotNull(response)) {
		int value = Integer.parseInt(response);
		if (value >= BroadBandTestConstants.CONSTANT_25 && value <= BroadBandTestConstants.CONSTANT_30)
		    status = true;

	    }

	    if (status) {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : power limit verified Successfully  ");
	    } else {
		LOGGER.info("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Set the band channel to " + channelValue);
	    LOGGER.info("POST-CONDITION 1: ACTION : Execute webpa/dmcli command to set band channel value to"
		    + channelValue);
	    LOGGER.info("POST-CONDITION 1: EXPECTED : Band channel value set to " + channelValue);

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
		    BroadBandTestConstants.CONSTANT_2, channelValue);
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : channelValue  reverted successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1: ACTUAL : Failed to set band channel to" + channelValue);
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 2: DESCRIPTION : Apply radio settings ");
	    LOGGER.info(
		    "POST-CONDITION 2: ACTION :Execute command :dmcli eRT setv Device.WiFi.Radio.2.X_CISCO_COM_ApplySetting bool true");
	    LOGGER.info("POST-CONDITION 2: EXPECTED :Settings applied");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("POST-CONDITION 2 : ACTUAL :Radio settings applied Successfully ");
	    } else {
		LOGGER.error("POST-CONDITION 2 : ACTUAL : Failed to apply radio settings ");
	    }
	    BroadBandPostConditionUtils.executePostConditionToStartMeshUsingSystemCommands(device, tapEnv,
		    meshInitialStatus, BroadBandTestConstants.CONSTANT_3);
	}
    }

    /**
     * Test case is created as part of Configuration of WiFi Tx Rate for 2.4GHz Radio and 5GHZ.
     *
     * Test Case # 1: Verify the configuration of Transmission Control Rate for both 2.4GHz bands and 5Ghz bands using
     * WebPA.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * /** Test case is created as part of Configuration of WiFi Tx Rate for 2.4GHz Radio and 5GHZ.
     *
     * Test Case # 1: Verify the configuration of Transmission Control Rate for both 2.4GHz bands and 5Ghz bands using
     * WebPA.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1)Verify the default value of operational transmission rate of the 2.4 GHz with operating standard as g/n
     * </li>
     * <li>S2)Verify the supported data transmit rate of the 2.4 GHz with operating standard as g/n</li>
     * <li>S3)Verify the default value of basic transmission rate of the 2.4 GHz with operating standard as g/n</li>
     * <li>S4)Verify the default value of operational transmission rate of the 2.4 GHz with operating standard as b/g/n
     * </li>
     * <li>S5)Verify the supported data transmit rate of the 2.4 GHz with operating standard as b/g/n</li>
     * <li>S6)Verify the default value of basic transmission rate of the 2.4 GHz with operating standard as b/g/n</li>
     * <li>S7)Verify setting unsupported values for operational transmission rate for 2.4GHz band.</li>
     * <li>S8)Verify the value of operational transmission rate of the 2.4 GHz.</li>
     * <li>S9)Verify setting values for operational transmission rate for 2.4GHz band excluding the basic transmission
     * rate.</li>
     * <li>S10)Verify the default value of operational transmission rate of the 2.4 GHz.</li>
     * <li>S11)Verify setting the operational transmission rate for 2.4GHz with operating standard as b/g/n with values
     * 1,2,6,9.</li>
     * <li>S12)Verify retrieving the operational transmission rate for 2.4GHz with operating standard as b/g/n.</li>
     * <li>S13)Verify retrieving the value of basic transmission rate of the 2.4 GHz with operating standard as b/g/n
     * </li>
     * <li>S14)Verify retrieving the OperatingStandards for 5GHz default value as 802.11a/n/ac</li>
     * <li>S15)Verify retrieving value of basic data transmission rate of 5GHz with operating standard as 802.11a/n/ac
     * </li>
     * <li>S16)Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as 802.11a/n/ac</li>
     * <li>S17)Verify retrieving the operational transmission rate for 5GHz with operating standard as 802.11a/n/ac</li>
     * <li>S18)Change the operating mode for 5GHz Wi-Fi for device to 802.11 n/ac and verify the wifi rates</li>
     * <li>S18)Verify retrieving the OperatingStandards for 5GHz</li>
     * <li>S20)Verify retrieving value of basic data transmission rate of 5GHz with operating standard as n/ac</li>
     * <li>S21)Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as 802.11 n/ac</li>
     * <li>S22)Verify the operational transmission rate for 5GHz with operating standard as 802.11 n/ac</li>
     * <li>S23)Change the operating mode for 5GHz Wi-Fi for device to 802.11ac and verify the wifi rates</li>
     * <li>S24)Verify the OperatingStandards for 5GHz by using webpa</li>
     * <li>S25)Verify the value of basic data transmission rate of 5GHz with operating standard as ac</li>
     * <li>S26)Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11 ac</li>
     * <li>S27)Verify retrieving the operational transmission rate for 5GHz with operating standard as 802.11ac</li>
     * <li>S28)Verify retrieving the operational transmission rate for 2.4GHz with operating standard as b/g/n post
     * reboot.</li>
     * <li>S29)Verify the supported data transmit rate of the 2.4 GHz with operating standard as b/g/n post reboot.</li>
     * <li>S30)Verify retrieving the value of basic transmission rate of the 2.4 GHz with operating standard as b/g/n
     * post reboot.</li>
     * <li>S31)Verify setting the operational transmission rate for 2.4GHz with operating standard as g/n with values
     * 6,9,12,18</li>
     * <li>S32)Verify retrieving the operational transmission rate for 2.4GHz with operating standard as g/n.</li>
     * <li>S33)Verify retrieving the value of basic transmission rate of the 2.4 GHz with operating standard as g/n</li>
     * <li>S34)Verify the beacon traffic use the lowest supported basic rate (2.4GHz)</li>
     * <li>S35)Verify the OperatingStandard for 5GHz by webpa post reboot</li>
     * <li>S36)Verify the value of basic data transmission rate of 5GHz with operating standard as '802.11ac' by webpa
     * post reboot</li>
     * <li>S37)Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11ac by webpa post reboot
     * </li>
     * <li>S38)Verify the operational transmission rate for 5GHz with operating standard as 802.11ac by webpa post
     * reboot</li>
     * <li>S39)Change the operating mode for 5GHz Wi-Fi for device to 802.11n and verify the wifi rates</li>
     * <li>S40)Verify retrieving the OperatingStandards for 5GHz by Webpa</li>
     * <li>S41)Verify the value of basic data transmission rate of 5GHz with operating standard as 'n'</li>
     * <li>S42)Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11n</li>
     * <li>S43)Verify the operational transmission rate for 5GHz with operating standard as 802.11n</li>
     * <li>Pre-Condition:Perform image upgrade on the device.</li>
     * <li>S44)Verify retrieving the operational transmission rate for 2.4GHz with operating standard as g/n post image
     * upgrade.</li>
     * <li>S45)Verify the supported data transmit rate of the 2.4 GHz with operating standard as g/n post image upgrade.
     * </li>
     * <li>S46)Verify retrieving the value of basic transmission rate of the 2.4 GHz with operating standard as g/n post
     * image upgrade.</li>
     * <li>S47)Verify the OperatingStandard for 5GHz with operating standard as 802.11n by webpa post upgrade</li>
     * <li>S48)Verify the value of basic data transmission rate of 5GHz with operating standard as '802.11n' by webpa
     * post upgrade</li>
     * <li>S49)Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11n by webpa post upgrade
     * </li>
     * <li>S50)Verify the operational transmission rate for 5GHz with operating standard as 802.11n by webpa post
     * upgrade</li>
     * <li>
     * </ol>
     * *
     * 
     * @author BALAJI V,JOSEPH M, INFOSYS
     * @author ArunKumar Jayachandran
     * @refactor Said Hisham
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-TX-RATE-CONFIG-5061")
    public void testTxRateConfigWebPa(Dut device) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-WIFI-TX-RATE-CONFIG-561";
	boolean result = false;
	String errorMessage = null;
	String step = null;
	String expectedValue = null;
	String currentFirmwareVersion = null;
	boolean hasBuildChanged = false;
	boolean status = false;
	long webPaWaitDuration = BroadBandTestConstants.ONE_MINUTE_IN_MILLIS;
	String response = null;
	int stepCounter = 0;
	int stepNumber = 0;
	// Variable declaration ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TX-RATE-CONFIG-5061");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the configuration of Transmission Control Rate for both 2.4 GHz & 5 GHz radio using WebPA.");
	LOGGER.info("#######################################################################################");
	try {

	    // Invoke Pre-Condition Method.
	    executePreConditions(tapEnv, device);

	    /**
	     * S1) Verify the default value of operational transmission rate of the 2.4GHz with operating standard as
	     * g/n or b/g/n for DSL devices.
	     */
	    stepNumber = BroadBandTestConstants.CONSTANT_1;
	    step = "S" + stepNumber;

	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.OPERATIONAL_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY DEFAULT VALUE OF OPERATIONAL TX RATE OF 2.4GHz (g/n) or b/g/n for DSL devices.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: execute webpa get operation for parameter Device.WiFi.Radio.10000.OperationalDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
	    errorMessage = "Unable to verify the 2.4GHz (g/n) or b/g/n for DSL devices Default Operating Tx Rate: Expected Value = "
		    + expectedValue + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : DEFAULT OPERATIONAL TX RATE OF 2.4GHz  (g/n) IS: "
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify the supported data transmit rate of the 2.4GHz with operating standard as g/n or b/g/n for DSL
	     * devices
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.SUPPORTED_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: VERIFY SUPPORTED TX RATE OF 2.4GHz (g/n).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation for parameter Device.WiFi.Radio.10000.SupportedDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES);
	    errorMessage = "Unable to verify the 2.4GHz (g/n) Supported Data Tx Rate: Expected Value = " + expectedValue
		    + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
			+ "SUPPORTED TX RATE OF 2.4GHz (g/n) or b/g/n for DSL devices is: " + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S3) Verify the default value of basic transmission rate of the 2.4GHz with operating standard as g/n or
	     * b/g/n for DSL devices
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.BASIC_TX_RATE_2GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: VERIFY DEFAULT VALUE OF BASIC TX RATE OF 2.4GHz (g/n).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation for parameter Device.WiFi.Radio.10000.BasicDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    DeviceModeHandler.isDSLDevice(device)
			    ? BroadBandWebPaConstants.WEBPA_PARAM_2_OR_5_DSL_DEVICE_BASIC_DATA_TRANSMIT_RATES.replace(
				    BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.RADIO_24_GHZ_INDEX)
			    : BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
	    errorMessage = "Unable to verify the 2.4GHz (g/n) (b/g/n for DSL devices)"
		    + " Default Basic Tx Rate: Expected Value = " + expectedValue + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : DEFAULT BASIC TX RATE OF 2.4GHz (g/n) IS: " + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || DeviceModeHandler.isDSLDevice(device)) {
		/**
		 * S4) Verify the default value of operational transmission rate of the 2.4GHz with operating standard
		 * as b/g/n.
		 */
		expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.SUPPORTED_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY DEFAULT VALUE OF OPERATIONAL TX RATE OF 2.4GHz (g/n/ax) for Ada & (b/g/n) for others.");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: webpa set opration for Device.WiFi.Radio.10000.OperatingStandards as b,g,n");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATING_STD, BroadBandTestConstants.CONSTANT_0,
			expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
				BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_2GHZ));

		errorMessage = "Unable to set the 2.4GHz Operating Standard to b/g/n.";
		if (result) {
		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		    errorMessage = "Unable to verify the 2.4GHz  (b/g/n) Default Basic Tx Rate: Expected Value = "
			    + expectedValue + " | Actual Value - " + response;
		    result = CommonMethods.isNotNull(response)
			    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		}
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + ": ACTUAL : DEFAULT OPERATIONAL TX RATE OF 2.4GHz for g/n/ax & b/g/n specific boxes are : "
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S5) Verify the supported data transmit rate of the 2.4GHz with operating standard as b/g/n
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.SUPPORTED_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: VERIFY SUPPORTED TX RATE OF 2.4GHz (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.SupportedDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz (b/g/n) Supported Data Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info(
			    "STEP " + stepNumber + ": ACTUAL : SUPPORTED TX RATE OF 2.4GHz (b/g/n) is:  " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S6) Verify the default value of basic transmission rate of the 2.4GHz with operating standard as
		 * b/g/n
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.BASIC_TX_RATE_2GHZ_FOR_DEVICES);
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY DEFAULT VALUE OF BASIC TX RATE OF 2.4GHz (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa get opration for Device.WiFi.Radio.10000.BasicDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			DeviceModeHandler.isDSLDevice(device)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_OR_5_DSL_DEVICE_BASIC_DATA_TRANSMIT_RATES
					.replace(BroadBandTestConstants.TR181_NODE_REF,
						BroadBandTestConstants.RADIO_24_GHZ_INDEX)
				: BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz  (g/n) Default Basic Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber + ": ACTUAL : DEFAULT BASIC TX RATE OF 2.4GHz (b/g/n) is:  "
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S7) Verify setting unsupported values Operational Transmission Rate for 2.4GHz band.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandTestConstants.UNSUPPORTED_OPERATION_TX_RATE_2GHZ;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY SETTING THE (UNSUPPORTED) VALUE OF OPERATIONAL TX RATE OF 2.4GHz  (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa set opration for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST NOT BE SET SUCCESSFULLY.");
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES, BroadBandTestConstants.CONSTANT_0,
			expectedValue);
		status = !result;
		if (result) {
		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		    status = CommonMethods.isNotNull(response)
			    && !BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		}
		errorMessage = "Able to set the Operational Tx Rates for 2.4GHz with Unsupported Values";
		if (status) {
		    LOGGER.info("STEP " + stepNumber
			    + ": ACTUAL : UNABLE TO SET THE UNSUPPORTED VALUES FOR OPERATIONAL TX RATE FOR 2.4GHz. ");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		/**
		 * S8) Verify setting values Operational Transmission Rate for 2.4GHz band excluding the basic
		 * transmission rate.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandTestConstants.NON_BASIC_OPERATIONAL_TX_RATE_2GHZ;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY SETTING THE (NON-BASIC) VALUE OF OPERATIONAL TX RATE OF 2.4GHz  (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa set opration for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST NOT BE SET SUCCESSFULLY.");
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES, BroadBandTestConstants.CONSTANT_0,
			expectedValue);
		if (result) {
		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		    result = CommonMethods.isNotNull(response)
			    && !BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		}
		errorMessage = "Able to set the Operational Tx Rates for 2.4GHz with Non-Basic Tx Rate Values";
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + ": ACTUAL : UNABLE TO SET THE NON-BASIC TX RATE VALUES FOR OPERATIONAL TX RATE FOR 2.4GHz.");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    } else {
		LOGGER.info(" skipping step 4 to 8 as device Supports only g & n Operating modes.");

		errorMessage = "";
		for (stepNumber = 4; stepNumber <= 8; stepNumber++) {
		    step = "S" + stepNumber;
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }

	    /**
	     * S9) Verify setting the operational transmission rate for 2.4GHz with operating standard as b/g/n with
	     * value 1,2,6,9.
	     */
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadBandTestConstants.TEMP_OPEATIONAL_TX_RATE_2GHZ;
		stepNumber = 9;
		step = "S" + stepNumber;
		result = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY SETTING THE VALUE OF OPERATIONAL TX RATE OF 2.4GHz  (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa set operation for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE SET SUCCESSFULLY.");
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES, BroadBandTestConstants.CONSTANT_0,
			expectedValue);
		errorMessage = "Unable to set the Operational Tx Rates for 2.4GHz with Operating Standard as b/g/n.";
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + ": ACTUAL : SUCCESSFULLY SET THE OPERATIONAL TX RATE FOR 2.4GHz WITH OPERATING STANDARD AS b/g/n");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S10) Verify retrieving the operational transmission rate for 2.4GHz with operating standard as b/g/n.
		 */
		expectedValue = BroadBandTestConstants.TEMP_OPEATIONAL_TX_RATE_2GHZ;
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY RETRIEVING OPERATIONAL TX RATE OF 2.4GHz (b/g/n).");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz  (b/g/n) Operational Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info(
			    "STEP " + stepNumber + ": ACTUAL : OPERATIONAL TX RATE OF 2.4GHz (b/g/n) IS: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = " In DSL Devices OPERATIONAL TX RATES cannot be overrided. So marking step 9 and 10 as NA.";
		LOGGER.error("errorMessage");
		for (stepNumber = 9; stepNumber <= 10; stepNumber++) {
		    step = "S" + stepNumber;
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }

	    /**
	     * S11) Verify retrieving the basic transmission rate for 2.4GHz with operating standard as b/g/n.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;

	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.BASIC_TX_RATE_2GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: VERIFY RETRIEVING BASIC TX RATE OF 2.4GHz (b/g/n).");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.BasicDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
	    errorMessage = "Unable to verify the 2.4GHz  (b/g/n) Basic Tx Rate: Expected Value = " + expectedValue
		    + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : BASIC TX RATE OF 2.4GHz (b/g/n) IS: " + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S12) Verify retrieving the OperatingStandards for 5GHz default value as 802.11a/n/ac.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_5GHZ);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify retrieving the OperatingStandards for 5GHz default value as 802.11a/n/ac ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP" + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    DeviceModeHandler.isDSLDevice(device) ? BroadBandWebPaConstants.DMCLI_COMMAND_WIRELESS_MODE_5GHZ
			    : BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
	    errorMessage = "Unable to retrieve the default OperatingStandards for 5GHz: Expected Value = "
		    + expectedValue + " | Actual Value - " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL:Default Operating standards for 5GHz is" + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S13) Verify retrieving value of basic data transmission rate of 5GHz with operating standard as
	     * 802.11a/n/ac
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    if (DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_5GHZ);

	    } else {
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_5GHZ, "6");
	    }

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP" + stepNumber
		    + ": DESCRIPTION: Verify retrieving value of basic data transmission rate of  5GHz with operating standard as 802.11a/n/ac");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
	    LOGGER.info("STEP" + stepNumber + ":EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    DeviceModeHandler.isDSLDevice(device)
			    ? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_OPERATINGSTANDARDS_IN_5GHZ
			    : BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
	    errorMessage = "Unable to verify the 5GHz(802.11a/n/ac) Default Basic Tx Rate: Expected Value = "
		    + expectedValue + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL:BASIC TX RATE with operating standard as 802.11a/n/ac is:"
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S14) Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as 802.11a/n/ac
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.SUPPORTED_TRANSMISSION_RATE_5GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP" + stepNumber + ": DESCRIPTION: Verify retrieving the SupportedDataTransmitRates for "
		    + "5GHz with operating standard as  802.11a/n/ac");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
	    LOGGER.info("STEP" + stepNumber + ":EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    DeviceModeHandler.isDSLDevice(device)
			    ? BroadBandWebPaConstants.WEBPA_PARAM_2_OR_5_DSL_DEVICE_BASIC_DATA_TRANSMIT_RATES.replace(
				    BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.RADIO_5_GHZ_INDEX)
			    : BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
	    errorMessage = "Unable to retrieve the SupportedDataTransmitRates for 5GHz with operating standard as  "
		    + "802.11a/n/ac: Expected Value = " + expectedValue + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber
			+ " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11a/n/ac is"
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S15) Verify retrieving the operational transmission rate for 5GHz with operating standard as
	     * 802.11a/n/ac.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.OPERATIONAL_TRANSMISSION_RATE_5GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP" + stepNumber + ": DESCRIPTION: Verify retrieving the operational transmission rate for "
		    + "5GHz with operating standard as 802.11a/n/ac");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
	    LOGGER.info("STEP" + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
	    errorMessage = "Unable to verify the 5GHz Default Operating Tx Rate: Expected Value = " + expectedValue
		    + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber
			+ " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11a/n/ac is:"
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    if (!(DeviceModeHandler.isDSLDevice(device))) {
		/**
		 * S16) Change the operating mode for 5GHz Wi-Fi for device to 802.11 n/ac and verify the wifi rates
		 * 
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		String operatingStandard = null;
		operatingStandard = BroadBandTestConstants.OPERATING_MODE_NAC;
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_N_AC;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Change the operating mode for 5GHz Wi-Fi for device to 802.11 n/ac and verify the wifi rates");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa set operation for Device.WiFi.Radio.10100.OperatingStandards as n,ac");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD, WebPaDataTypes.STRING.getValue(),
			operatingStandard, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		errorMessage = "Unable to set the 5GHz Operating Standard to n/ac.";
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:Changed the operating mode for  5GHz Wi-Fi for device  to 802.11 n/ac");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S17) Verify retrieving the OperatingStandards for 5GHz.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		tapEnv.waitTill(webPaWaitDuration);
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_N_AC;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber + ": DESCRIPTION: Verify retrieving the OperatingStandards for 5GHz");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP" + stepNumber + "EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
		errorMessage = "Unable to retrieving the OperatingStandards for 5GHz: Expected Value = " + expectedValue
			+ " | Actual Value - " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber + " ACTUAL:operating standards for 5GHz is" + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S18) Verify retrieving value of basic data transmission rate of 5GHz with operating standard as n/ac
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_5GHZ, "6");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Verify  retrieving value of basic data transmission rate of  5GHz with operating standard as n/ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 5GHz(n/ac) Default Basic Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + ": ACTUAL: BASIC TX RATE with operating standard as 802.11 n/ac is:" + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S19) Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as 802.11 n/ac
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.SUPPORTED_DATA_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as  802.11 n/ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
		errorMessage = "Unable to retrieve the SupportedDataTransmitRates for 5GHz with operating standard as  802.11 n/ac: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11 n/ac is"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S20) Verify the operational transmission rate for 5GHz with operating standard as 802.11 n/ac.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.OPERATIONAL_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Verify the operational transmission rate for 5GHz with operating standard as 802.11 n/ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 5GHz  Operating Tx Rate: Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11 n/ac is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S21) Change the operating mode for 5GHz Wi-Fi for device to 802.11ac and verify the wifi rates
		 * 
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_AC;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Change the operating mode for 5GHz Wi-Fi for device to 802.11ac and verify the wifi rates");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa set operation for Device.WiFi.Radio.10100.OperatingStandards as ac");
		LOGGER.info("STEP" + stepNumber + ":EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD, WebPaDataTypes.STRING.getValue(),
			expectedValue, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		errorMessage = "Unable to set the 5GHz Operating Standard to 'ac' mode.";
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:Changed the operating mode for  5GHz Wi-Fi for device  to 802.11ac");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S22) Verify the OperatingStandards for 5GHz by using webpa
		 * 
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		tapEnv.waitTill(webPaWaitDuration);
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_AC;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP" + stepNumber + ": DESCRIPTION: Verify the OperatingStandards for 5GHz by using webpa");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
		errorMessage = "Unable to verify the OperatingStandards for 5GHz with operating standard: Expected Value = "
			+ expectedValue + " | Actual Value - " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber
			    + " ACTUAL:operating standards for 5GHz with operating standard as  802.11ac is"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S23) Verify the value of basic data transmission rate of 5GHz with operating standard as ac
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_5GHZ, "6");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Verify the value of basic data transmission rate of 5GHz with operating standard as ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 5GHz(ac) Basic Tx Rate : Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber + " ACTUAL:BASIC TX RATE with operating standard as 802.11 ac is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S24) Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11 ac
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.SUPPORTED_DATA_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: Verify the SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
		errorMessage = "Unable to verify the SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber
			    + " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac is"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S25) Verify retrieving the operational transmission rate for 5GHz with operating standard as
		 * 802.11ac.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.OPERATIONAL_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP" + stepNumber
			+ ": DESCRIPTION: VERIFY VALUE OF OPERATIONAL TX RATE OF 5GHz WITH OPERATING STANDARD AS 802.11ac");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
		LOGGER.info("STEP" + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 5GHz  Operating Tx Rate: Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber
			    + " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11ac is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    } else {
		for (stepCounter = 16; stepCounter <= 25; stepCounter++) {
		    step = "s" + stepCounter;
		    errorMessage = "This Step " + step + " is not Applicable for DSL devices";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }

	    /**
	     * S26) Verify retrieving the operational transmission rate for 2.4GHz with operating standard as b/g/n post
	     * re-boot.
	     */
	    stepNumber = 26;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.TEMP_OPERATIONAL_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY RETRIEVING OPERATIONAL TX RATE OF 2.4GHz (b/g/n) POST REBOOT.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute 1. /sbin/reboot 2. webpa get operation for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    boolean isRebooted = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    errorMessage = "Unable to reboot the device.";
	    if (isRebooted) {
		isRebooted = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		errorMessage = "Unable to verify WebPA is up & running";
	    }
	    if (isRebooted) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		errorMessage = "Unable to verify the 2.4GHz (b/g/n) Operational Tx Rate (Post Reboot): Expected Value - "
			+ expectedValue + ", Actual Value -" + response;
		if (result) {
		    LOGGER.info("STEP" + stepNumber
			    + " ACTUAL: OPERATIONAL TX RATE OF 2.4GHz (b/g/n) (POST REBOOT) is: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S27) Verify the supported data transmit rate of the 2.4GHz with operating standard as b/g/n post re-boot
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
		    BroadBandTestConstants.OPERATION_TX_RATE_B_G_N,
		    BroadbandPropertyFileHandler.getSupportedTxRate2GhzForDevice());

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION: VERIFY SUPPORTED TX RATE OF 2.4GHz (b/g/n) POST REBOOT.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.SupportedDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    if (isRebooted) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz (b/g/n) Supported Data Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber + " ACTUAL: SUPPORTED TX RATE OF 2.4GHz (b/g/n) (POST REBOOT) is: "
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S28) Verify retrieving the basic transmission rate for 2.4GHz with operating standard as b/g/n post
	     * re-boot.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.TEMP_BASIC_TX_RATE_2GHZ_FOR_DEVICES);

	    errorMessage = "Unable to verify the 2.4GHz  (b/g/n) Basic Tx Rate (Post Reboot): Expected Value = "
		    + expectedValue + " | Actual Value = " + response;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY RETRIEVING BASIC TX RATE OF 2.4GHz (b/g/n) POST REBOOT.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.BasicDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    if (isRebooted) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		errorMessage = "Unable to verify the 2.4GHz  (b/g/n) Basic Tx Rate (Post Reboot): Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		if (result) {
		    LOGGER.info("STEP" + stepNumber + " ACTUAL: BASIC TX RATE OF 2.4GHz (b/g/n) (POST REBOOT) is: "
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    if (!(DeviceModeHandler.isDSLDevice(device))) {
		/**
		 * S29) Verify setting the operational transmission rate for 2.4GHz with operating standard as g/n with
		 * values 6,9,12,18
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandTestConstants.TEMP_OPRATIONAL_TX_RATE_G_N;
		String operationalStandard = BroadBandTestConstants.WIFI_OPERATING_STANDARD_G_N;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY SETTING THE VALUE OF OPERATIONAL TX RATE OF 2.4GHz  (g/n).");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa set operation for Device.WiFi.Radio.10000.OperatingStandards as g,n");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE SET SUCCESSFULLY.");
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATING_STD, BroadBandTestConstants.CONSTANT_0,
			operationalStandard);
		errorMessage = "Unable to set the 2.4GHz Operating Standard to g/n.";
		if (result) {
		    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES,
			    BroadBandTestConstants.CONSTANT_0, expectedValue);
		    errorMessage = "Unable to set the Operational Tx Rates for 2.4GHz with Operating Standard as g/n.";
		}
		if (result) {
		    LOGGER.info("STEP" + stepNumber
			    + " ACTUAL: SUCCESSFULLY SET THE OPERATIONAL TX RATE FOR 2.4GHz WITH OPERATING STANDARD AS g/n");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S30) Verify retrieving the operational transmission rate for 2.4GHz with operating standard as g/n.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandTestConstants.TEMP_OPRATIONAL_TX_RATE_G_N;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP " + stepNumber + ": DESCRIPTION: VERIFY RETRIEVING OPERATIONAL TX RATE OF 2.4GHz (g/n).");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz  (g/n) Operational Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber + " ACTUAL: OPERATIONAL TX RATE OF 2.4GHz (g/n) is: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S31) Verify retrieving the basic transmission rate for 2.4GHz with operating standard as g/n.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_G_N, "6");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: VERIFY RETRIEVING BASIC TX RATE OF 2.4GHz (g/n).");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.BasicDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz  (g/n) Basic Tx Rate: Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP" + stepNumber + " ACTUAL: BASIC TX RATE OF 2.4GHz (g/n) is: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		for (stepCounter = 29; stepCounter <= 31; stepCounter++) {
		    step = "s" + stepCounter;
		    errorMessage = "This Step " + step + " is not Applicable for DSL ,and few specific box models";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}

	    }

	    /**
	     * S32) Verify the beacon traffic use the lowest supported basic rate (2.4GHz)
	     */
	    stepNumber = 32;
	    step = "S" + stepNumber;
	    result = false;
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadBandTestConstants.TEXT_SIX_MBPS;
	    } else {
		expectedValue = BroadBandTestConstants.TEXT_ONE_MBPS;
	    }

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ":  DESCRIPTION: VERIFY BEACON TRAFFIC USE LOWEST SUPPORTED BASIC RATE (2.4GHz).");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_BeaconRate");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    DeviceModeHandler.isDSLDevice(device)
			    ? BroadBandWebPaConstants.WEBPA_PARAM_BEACON_DSL_DEVICE_OPERATING_STD.replace(
				    BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.WIFI_5_GHZ_INDEX)
			    : BroadBandWebPaConstants.WEBPA_PARAM_2_4_BEACON_RATE);
	    errorMessage = "Unable to verify the Beacon Rate (2.4GHz): Expected Value = " + expectedValue
		    + " | Actual Value = " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareValues("TXT_COMPARISON", expectedValue, response);
	    if (result) {
		LOGGER.info("STEP" + stepNumber + " ACTUAL: BEACON TRAFFIC USES TX RATE: " + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S33) Verify the OperatingStandard for 5GHz by webpa post reboot.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    if (DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC;
	    } else {
		expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_5GHZ);
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ":  DESCRIPTION: Verify the OperatingStandard for 5GHz by webpa post reboot.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
	    errorMessage = "Unable to verify the OperatingStandards for 5GHz by webpa post reboot: Expected Value = "
		    + expectedValue + " | Actual Value - " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL:operating standards for 5GHz by webpa post reboot is-"
			+ response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    if (!(DeviceModeHandler.isDSLDevice(device))) {
		/**
		 * S34) Verify the value of basic data transmission rate of 5GHz with operating standard as '802.11ac'
		 * by webpa post reboot
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_5GHZ, "6");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: Verify  the value of basic data transmission rate of  5GHz with operating standard as '802.11ac' by webpa post reboot");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 5GHz(802.11ac)  Basic Tx Rate by webpa post reboot: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:BASIC TX RATE with operating standard as 802.11ac by webpa post reboot is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S35) Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11ac by webpa post
		 * reboot
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.SUPPORTED_DATA_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: Verify the SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac by webpa post reboot");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
		errorMessage = "Unable to retrieve the SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac by webpa post reboot : Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11ac is by webpa post reboot"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S36) Verify the operational transmission rate for 5GHz with operating standard as 802.11ac by webpa
		 * post reboot.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.OPERATIONAL_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedDataTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY THE OPERATIONAL TX RATE OF 5GHz WITH OPERATING STANDARD AS 802.11ac by webpa post reboot");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 5GHz  Operating Tx Rate by webpa post reboot: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11ac  by webpa post reboot is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S37) Change the operating mode for 5GHz Wi-Fi for device to 802.11n and verify the wifi rates
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = DeviceModeHandler.isDSLDevice(device)
			? BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC
			: BroadBandTestConstants.OPERATING_STANDARDS_N;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: Change the operating mode for  5GHz Wi-Fi for device  to 802.11n and verify the wifi rates");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD, WebPaDataTypes.STRING.getValue(),
			expectedValue, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		errorMessage = "Unable to set the 5GHz Operating Standard to 802.11n.";
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:Changed the operating mode for  5GHz Wi-Fi for device  to 802.11n");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		for (stepCounter = 34; stepCounter <= 37; stepCounter++) {
		    step = "s" + stepCounter;
		    errorMessage = "This Step " + step + " is not Applicable for DSL AND some other specific models";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }

	    /**
	     * S38) Verify retrieving the OperatingStandards for 5GHz by Webpa.
	     */
	    stepNumber = 38;
	    step = "S" + stepNumber;
	    result = false;
	    tapEnv.waitTill(webPaWaitDuration);
	    expectedValue = DeviceModeHandler.isDSLDevice(device) ? BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC
		    : BroadBandTestConstants.OPERATING_STANDARDS_N;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION: Verify retrieving the OperatingStandards for 5GHz by Webpa");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
	    errorMessage = "Unable to verify the OperatingStandards for 5GHz with operating standard: Expected Value = "
		    + expectedValue + " | Actual Value - " + response;
	    result = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
	    if (result) {
		LOGGER.info("STEP " + stepNumber + " ACTUAL:operating standards for 5GHz is" + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    if (!(DeviceModeHandler.isDSLDevice(device))) {
		/**
		 * S39) Verify the value of basic data transmission rate of 5GHz with operating standard as 'n'
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.BASIC_TX_RATE_5GHZ, "6");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: Verify the value of basic data transmission rate of 5GHz with operating standard as 'n'");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 5GHz(802.11n)  Basic Tx Rate: Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber + " ACTUAL:BASIC TX RATE with operating standard as 802.11n is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S40) Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11n
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.SUPPORTED_DATA_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getSupportedTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: Verify  the SupportedDataTransmitRates for 5GHz with operating standard as 802.11n");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
		errorMessage = "Unable to retrieve the SupportedDataTransmitRates for 5GHz with operating standard as  802.11n: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11n is"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		/**
		 * S41) Verify the operational transmission rate for 5GHz with operating standard as 802.11n.
		 */
		stepNumber++;
		step = "S" + stepNumber;
		result = false;
		expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
			BroadBandTestConstants.OPERATIONAL_TX_RATE_5GHZ,
			BroadbandPropertyFileHandler.getOperationalTxRate5GhzForDevice());
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION: VERIFY THE VALUE OF OPERATIONAL TX RATE OF 5GHz WITH OPERATING STANDARD AS 802.11n");
		LOGGER.info("STEP" + stepNumber
			+ ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 5GHz  Operating Tx Rate: Expected Value = " + expectedValue
			+ " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11n is:" + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		for (stepCounter = 39; stepCounter <= 41; stepCounter++) {
		    step = "s" + stepCounter;
		    errorMessage = "This Step " + step
			    + " is not Applicable for DSL devices and few specific box models";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }

	    /**
	     * S42) Verify retrieving the operational transmission rate for 2.4GHz with operating standard as g/n (b,g,n
	     * for DSL) post image upgrade.
	     */
	    stepNumber = 42;
	    step = "S" + stepNumber;
	    result = false;
	    if (DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadBandTestConstants.OPERATION_TX_RATE_B_G_N_DSL_DEVICE;
	    } else
		expectedValue = BroadBandTestConstants.TEMP_OPRATIONAL_TX_RATE_G_N;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY RETRIEVING OPERATIONAL TX RATE OF 2.4GHz (g/n) (b,g,n for DSL devices)"
		    + " POST IMAGE UPGRADE.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.OperationalDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");

	    currentFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + currentFirmwareVersion);

	    String latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
	    if (CommonMethods.isNull(latestImage)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImage);
	    }

	    if (CommonMethods.isNotNull(latestImage)) {

		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, latestImage, true,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		errorMessage = "Unable to trigger CDL latest build - " + latestImage;
		LOGGER.info("Triggering CDL using TR181");
		hasBuildChanged = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);
	    }

	    if (hasBuildChanged) {
		LOGGER.info("WEBPA PROCESS IS UP & RUNNING: "
			+ BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true));
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_OPERATIONAL_TX_RATES);
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		errorMessage = "Unable to verify the 2.4GHz (g/n) (b,g,n for DSL) Operational Tx Rate "
			+ "(Post Image Upgrade): Expected Value - " + expectedValue + ", Actual Value -" + response;
		if (result) {
		    LOGGER.info("STEP " + stepNumber + " ACTUAL: OPERATIONAL TX RATE OF 2.4GHz (g/n) (b,g,n for DSL)"
			    + " (POST IMAGE UPGRADE) IS: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "Unable to perform image upgrade on the device.";
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S43) Verify the supported data transmit rate of the 2.4GHz with operating standard as g/n (b,g,n for DSL)
	     * post image upgrade.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;

	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.TEMP_SUPPORTED_TRANSMISSION_RATE_2GHZ_FOR_DEVICES);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY SUPPORTED TX RATE OF 2.4GHz (g/n) (b,g,n for DSL) POST IMAGE UPGRADE.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.SupportedDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_2_4_SUPPORTED_DATA_TX_RATES);
		errorMessage = "Unable to verify the 2.4GHz (g/n) Supported Data Tx Rate: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL: SUPPORTED TX RATE OF 2.4GHz (g/n) (b,g,n for DSL) POST IMAGE UPGRADE is: "
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "Unable to perform image upgrade on the device.";
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S44) Verify retrieving the basic transmission rate for 2.4GHz with operating standard as g/n (b,g,n for
	     * DSL) post image upgrade..
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.BASIC_TX_RATE_2GHZ_FOR_DEVICES_POST_UPGRADE);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY RETRIEVING BASIC TX RATE OF 2.4GHz (g/n) (b,g,n for DSL) POST IMAGE UPGRADE.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10000.BasicDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to perform image upgrade on the device.";
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			DeviceModeHandler.isDSLDevice(device)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_OR_5_DSL_DEVICE_BASIC_DATA_TRANSMIT_RATES
					.replace(BroadBandTestConstants.TR181_NODE_REF,
						BroadBandTestConstants.RADIO_24_GHZ_INDEX)
				: BroadBandWebPaConstants.WEBPA_PARAM_2_4_BASIC_TX_RATES);
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		errorMessage = "Unable to verify the 2.4GHz (g/n) (b,g,n for DSL) Basic Tx Rate (Post Image Upgrade):"
			+ " Expected Value = " + expectedValue + " | Actual Value = " + response;
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL: BASIC TX RATE OF 2.4GHz (g/n) (POST IMAGE UPGRADE) is: " + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S45) Verify the OperatingStandard for 5GHz with operating standard as 802.11n (a,n,ac for DSL) by webpa
	     * post upgrade.
	     * 
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    if (DeviceModeHandler.isDSLDevice(device)) {
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_A_N_AC;
	    } else {
		expectedValue = BroadBandTestConstants.OPERATING_STANDARDS_N;
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify the  OperatingStandards for 5GHz   by webpa post upgrade.");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to perform image upgrade on the device.";
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATING_STD);
		errorMessage = "Unable to retrieving the OperatingStandards for 5GHz   by webpa post upgrade: Expected Value = "
			+ expectedValue + " | Actual Value - " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber + " ACTUAL:operating standards for 5GHz  by webpa post upgrade="
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S46) Verify the value of basic data transmission rate of 5GHz with operating standard as '802.11n'
	     * (a,n,ac for DSL) by webpa post upgrade
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.BASIC_TX_RATE_5GHZ_FOR_DEVICES_POST_UPGRADE);

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify  the value of basic data transmission rate of  5GHz with operating standard"
		    + " as '802.11n' (802.1a,n,ac for DSL) by webpa post upgrade");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.BasicDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			DeviceModeHandler.isDSLDevice(device)
				? BroadBandWebPaConstants.WEBPA_PARAM_2_OR_5_DSL_DEVICE_BASIC_DATA_TRANSMIT_RATES
					.replace(BroadBandTestConstants.TR181_NODE_REF,
						BroadBandTestConstants.RADIO_5_GHZ_INDEX)
				: BroadBandWebPaConstants.WEBPA_PARAM_5_BASIC_TX_RATES);
		errorMessage = "Unable to verify the 5GHz(n)  Basic Tx Rate by webpa post upgrade: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:BASIC TX RATE with operating standard as 802.11n by webpa post upgrade is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S47) Verify the SupportedDataTransmitRates for 5GHz with operating standard as 802.11n by (a,n,ac for
	     * DSL) webpa post upgrade
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
		    BroadBandTestConstants.SUPPORTED_DATA_TX_RATE_5GHZ,
		    BroadbandPropertyFileHandler.getSupportedTxRate5GhzForDevice());
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify retrieving the SupportedDataTransmitRates for 5GHz with operating standard as  802.11n by webpa post upgrade");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.SupportedDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to perform image upgrade on the device.";
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_SUPPORTED_DATA_TRANSMIT_TX_RATES);
		errorMessage = "Unable to retrieve the SupportedDataTransmitRates for 5GHz with operating standard as  802.11n by webpa post upgrade : Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:SupportedDataTransmitRates for 5GHz with operating standard as  802.11n  by webpa post upgrade"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    /**
	     * S48) Verify the operational transmission rate for 5GHz with operating standard as 802.11n (a,n,ac for
	     * DSL) by webpa post upgrade.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;
	    expectedValue = BroadBandCommonUtils.getTxRateExpctdValForDeviceType(device,
		    BroadBandTestConstants.OPERATIONAL_TX_RATE_5GHZ,
		    BroadbandPropertyFileHandler.getOperationalTxRate5GhzForDevice());
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: VERIFY THE VALUE OF OPERATIONAL TX RATE OF 5GHz WITH OPERATING STANDARD AS 802.11n by webpa post upgrade");
	    LOGGER.info("STEP" + stepNumber
		    + ": ACTION: Execute webpa get operation for Device.WiFi.Radio.10100.OperationalDataTransmitRates");
	    LOGGER.info("STEP " + stepNumber + "EXPECTED : VALUE MUST BE " + expectedValue);
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to perform image upgrade on the device.";
	    if (hasBuildChanged) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_5_OPERATIONAL_TX_RATES);
		errorMessage = "Unable to verify the 5GHz  Operating Tx Rate by webpa post upgrade: Expected Value = "
			+ expectedValue + " | Actual Value = " + response;
		result = CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.compareCommaSeparateValues(expectedValue, response);
		if (result) {
		    LOGGER.info("STEP " + stepNumber
			    + " ACTUAL:OPERATIONAL TX RATE OF 5GHz  with operating standard as 802.11n  by webpa post upgrade is:"
			    + response);
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	} catch (

	Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS: ");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION: FLASH THE BUILD ON THE DEVICE USING HTTP/ TR-181.");
	    LOGGER.info("POST-CONDITION 1: ACTION: Do CDL using Firmware " + currentFirmwareVersion);
	    LOGGER.info("POST-CONDITION 1: EXPTECTED: IMAGE MUST BE FLASHED ON THE DEVICE.");
	    LOGGER.info("**********************************************************************************");
	    if (!hasBuildChanged) {
		LOGGER.info(
			"BUILD CHANGED FLAG IS FALSE. AS A RE-VERIFICATION, THE CURRENT BUILD NAME & ORIGINAL BUILD NAME WILL BE VERIFIED.");
		String tempFwVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		LOGGER.info("CURRENT FIRMWARE VERSION: " + tempFwVersion);
		LOGGER.info("ORIGINAL FIRMWARE VERSION: " + currentFirmwareVersion);
		hasBuildChanged = !tempFwVersion.equalsIgnoreCase(currentFirmwareVersion);
	    }
	    if (hasBuildChanged) {
		try {
		    result = FirmwareDownloadUtils.triggerHttpCdlUsingTr181(tapEnv, device, currentFirmwareVersion);
		    // In case the HTTP TR-181 CDL Fails, then trigger CDL using TFTP Docsis SNMP Command.
		    if (!result) {
			LOGGER.error(
				"HTTP TR-181 CDL FAILED; HENCE GOING TO TRIGGER CDL WITH TFTP DOCSIS SNMP COMMANDS.");
			result = FirmwareDownloadUtils.triggerAndWaitForTftpCodeDownloadUsingDocsisSnmpCommand(tapEnv,
				device, currentFirmwareVersion + BroadBandCdlConstants.BIN_EXTENSION, false);
		    }
		    LOGGER.info("POST-CONDITION ACTUAL: FLASHED THE BUILD ON THE DEVICE: " + result);
		} catch (TestException testException) {
		    // Log & Suppress the exception
		    LOGGER.error(testException.getMessage());
		}
	    } else {
		LOGGER.info("SKIPPING FLASHING THE ORIGINAL BUILD AS THE IMAGE UPGRADE WAS NOT SUCCESSFUL.");
	    }
	    // Device Reactivation
	    performFactoryResetAndActivatation(tapEnv, device);
	    LOGGER.info("COMPLETED TEST CASE: TC-RDKB-WIFI-TX-RATE-CONFIG-5061");
	    LOGGER.info("#######################################################################################");
	}
    }
    
    /**
     * The method to be executed once during the execution of the test case; factory reset is being performed here.
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     */
    private void executePreConditions(AutomaticsTapApi tapEnv, Dut device) {
	String errorMessage = null;
	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS:");
	// Factory Reset
	LOGGER.info("PRE-CONDITION STEP 1: PERFORM FACTORY RESET ON THE DEVICE.");
	LOGGER.info("PRE-CONDITION DESCRIPTION: PERFORM FACTORY RESET USING WEBPA.");
	LOGGER.info(
		"PRE-CONDITION ACTION: Execute webpa set operation for Device.X_CISCO_COM_DeviceControl.FactoryReset");
	LOGGER.info("PRE-CONDITION EXPECTED: DEVICE MUST UNDERGO FACTORY RESET.");
	if (!performedFactoryResetInitTest) {
	    performedFactoryResetInitTest = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv,
		    device, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS);
	    LOGGER.info(
		    "PRE-CONDITION ACTUAL : FACTORY RESET SUCCESSFULLY PERFORMED: " + performedFactoryResetInitTest);
	    if (!performedFactoryResetInitTest) {
		errorMessage = "UNABLE TO PERFORM WIFI FACTORY RESET OPERATION ON THE DEVICE. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	} else {
	    LOGGER.info("PRE-CONDITION ACTUAL : FACTORY RESET PERFORMED ALREADY. HENCE SKIPPING THIS STEP.");
	}
	// WebPA Process Status
	LOGGER.info("PRE-CONDITION STEP 3: WEBPA PROCESS IS UP & RUNNING: "
		+ BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true));
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
    }
    
    /**
     * The method to be executed after test method where the factory reset needs to be performed on the device and then
     * the activation.
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi}
     * @param device
     *            {@link Dut}
     */
    private void performFactoryResetAndActivatation(AutomaticsTapApi tapEnv, Dut device) {
	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	LOGGER.info("POST-CONDITION STEP 1: PERFORM FACTORY RESET ON THE DEVICE.");
	LOGGER.info("POST-CONDITION DESCRIPTION: PERFORM FACTORY RESET USING WEBPA.");
	LOGGER.info(
		"POST-CONDITION ACTION: Execute webpa set operation for Device.X_CISCO_COM_DeviceControl.FactoryReset");
	LOGGER.info("POST-CONDITION EXPTECTED: DEVICE MUST UNDERGO FACTORY RESET.");
	boolean result = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
		BroadBandTestConstants.SIX_MINUTE_IN_MILLIS);
	LOGGER.info("POST-CONDITION ACTUAL : FACTORY RESET SUCCESSFULLY PERFORMED: " + result);
	LOGGER.info("POST-CONDITION ACTUAL: WEBPA PROCESS IS UP & RUNNING: "
		+ BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true));
	LOGGER.info("POST-CONDITION STEP 2: PERFORM DEVICE REACTIVATION USING WEBPA.");
	LOGGER.info("POST-CONDITION DESCRIPTION: REACTIVATE THE DEVICE WITH APPROPRIATE WIFI PARAMETERS.");
	LOGGER.info("POST-CONDITION ACTION: Perform Webpa or Snmp to reactive the device");
	LOGGER.info("POST-CONDITION EXPTECTED: DEVICE MUST BE REACTIVATTED.");
	BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
	LOGGER.info("POST-CONDITION STEP 2: DEVICE REACTIVATION COMPLETED USING WEBPA.");
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    }
}
